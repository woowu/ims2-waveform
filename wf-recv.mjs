#!/usr/bin/node

/* Receive waveform streaming from TCP. Each message
 * is in the form:
 *   0x3e | seqno | 64bit timestamp | 32 bit len | waveform chunk
 */

import path from 'node:path';
import fs from 'node:fs';
import { exec } from 'node:child_process';
import net from 'node:net';
import yargs from 'yargs/yargs';
import dump from 'buffer-hexdump';
import moment from 'moment';

const WF_SAMPLE_RATE = 6.4e3;
const WF_FRAME_SYNC = 0xa5;
const WF_FRAME_HEAD_LEN = 1;
const WF_FRAME_PAYLOAD_LEN = (2 + 2) * 3; /* u/i from 3 channels */
const WF_FRAME_HEAD_CS_LEN = 2;
const WF_FRAME_LEN = WF_FRAME_HEAD_LEN + WF_FRAME_PAYLOAD_LEN + WF_FRAME_HEAD_CS_LEN;
const MESSAGE_START = 0x3e;
const MESSAGE_START_LEN = 1;
const MESSAGE_SEQNO_LEN = 4;
const MESSAGE_TIMESTAMP_LEN = 8;
const MESSAGE_LEN_SZ = 4;
const MESSAGE_HEAD_LEN = MESSAGE_START_LEN + MESSAGE_SEQNO_LEN + MESSAGE_TIMESTAMP_LEN + MESSAGE_LEN_SZ;
const MAX_ROWS_EACH_FILE = 1.92e6;
const FILE_HANDLER = 'wf-overview.R';

function putData(workpad, data)
{
    workpad.inQueue.push(data);
    if (! workpad.parsing) {
        workpad.parsing = true;
        setImmediate(() => {
            parseStream(workpad);
        });
    }
}

function parseStream(workpad)
{
    const handleFrame = (frame, frameCounter) => {
        //console.log(frameCounter + ':', dump(frame).slice(10, 10 + 2*15 + 7));
        var cs = [0, 0];
        var i = 0;
        for (const b of frame.slice(WF_FRAME_HEAD_LEN
            , WF_FRAME_HEAD_LEN + WF_FRAME_PAYLOAD_LEN)) {
            cs[i % 2] ^= b;
            ++i;
        }
        if (cs[0] != frame[WF_FRAME_HEAD_LEN + WF_FRAME_PAYLOAD_LEN]
            || cs[1] != frame[WF_FRAME_HEAD_LEN + WF_FRAME_PAYLOAD_LEN + 1]) {
            console.error('bad frame: ', frameCounter);
            return false;
        }

        var line = `${frameCounter},`
            + `${moment(workpad.timestamp).format('YYYY-MM-DD HH:mm:ss.SSS')}`;
        var pos = WF_FRAME_HEAD_LEN;
        var value;
        for (var channel = 0; channel < 3; ++channel) {
            for (var q = 0; q < 2; ++q) {
                value = new DataView(new Uint8Array(
                    frame.slice(pos, pos + 2)
                ).buffer).getInt16(0, true);
                line += ',' + value;
                pos += 2;
            }
        }
        writeCsvRow(line, workpad);
        return true;
    };

    const parsePayload = payload => {
        const data = [...workpad.remainingPayload, ...payload];
        var pos = 0;

        /* Here only try to parse whole frames, for last incompleted
         * frame, leave it for the next message.
         */
        var last_success = true;
        var last_successful_pos = 0;
        while (data.length - pos >= WF_FRAME_LEN) {
            if (data[pos] == WF_FRAME_SYNC
                    && handleFrame(data.slice(pos, pos + WF_FRAME_LEN),
                        workpad.frameCounter)) {
                pos += WF_FRAME_LEN;
                last_successful_pos = pos;
                ++workpad.frameCounter;
            } else {
                ++pos;
                /* all the continuous not-consumed bytes are counted as a
                 * single bad frame, which should occupy a counter.
                 */
                if (last_success) {
                    ++workpad.frameCounter;
                    last_success = false;
                }
            }
        }
        workpad.remainingPayload = last_successful_pos
            ? data.slice(last_successful_pos) : [];
    };

    const onEnd = () => {
        workpad.parsing = false;
        if (workpad.inQueue.length)
            setImmediate(() => {
                parseStream(workpad);
            });
    };

    workpad.messageStream.push(...workpad.inQueue.shift());

    /* parse and strip a whole message, otherwise keep the stream
     * data untouched.
     */

    if (workpad.messageStream.length < MESSAGE_HEAD_LEN) return;
    if (workpad.messageStream[0] != MESSAGE_START) {
        console.error(dump(workpad.messageStream).slice(0, 64));
        throw new Error('bad message');
    }
    const messageSeqno = new DataView(
        new Uint8Array(workpad.messageStream.slice(
            MESSAGE_START_LEN, MESSAGE_START_LEN + MESSAGE_SEQNO_LEN)).buffer)
        .getUint32(0)
    const payloadLen = new DataView(
        new Uint8Array(workpad.messageStream.slice(
            MESSAGE_START_LEN + MESSAGE_SEQNO_LEN + MESSAGE_TIMESTAMP_LEN
            , MESSAGE_START_LEN + MESSAGE_SEQNO_LEN + MESSAGE_TIMESTAMP_LEN + MESSAGE_LEN_SZ)).buffer)
        .getUint32(0)
    if (workpad.messageStream.length < MESSAGE_HEAD_LEN + payloadLen) {
        onEnd();
        return;
    }

    if (workpad.messageSeqno === null) workpad.messageSeqno = messageSeqno;
    if (messageSeqno != workpad.messageSeqno)
        throw new Error(`incorrect message seqno. received ${messageSeqno}` +
            ` expected ${workpad.messageSeqno}`)
    workpad.timestamp = new Date(Number(new DataView(
        new Uint8Array(workpad.messageStream.slice(MESSAGE_START_LEN + MESSAGE_SEQNO_LEN
            , MESSAGE_START_LEN + MESSAGE_SEQNO_LEN + MESSAGE_TIMESTAMP_LEN)).buffer)
        .getBigInt64()))
    const payload = workpad.messageStream.slice(MESSAGE_HEAD_LEN, MESSAGE_HEAD_LEN + payloadLen);
    workpad.messagePrinter(messageSeqno, payload, workpad.frameCounter);
    workpad.messageStream = workpad.messageStream.slice(MESSAGE_HEAD_LEN + payloadLen);
    ++workpad.messageSeqno;
    parsePayload(payload);

    onEnd();
}

const argv = yargs(process.argv.slice(2))
    .option({
        'h': {
            alias: 'host',
            describe: 'remote host',
            type: 'string',
        },
        'p': {
            alias: 'port',
            describe: 'remote tcp port',
            type: 'number',
        },
        'o': {
            alias: 'out',
            describe: 'output filename for saving csv or raw stream',
            type: 'string',
        },
        'r': {
            alias: 'raw',
            describe: 'saving raw stream instead of csv',
            type: 'boolean',
        },
        'i': {
            alias: 'input',
            describe: 'input raw filename',
            type: 'string',
        },
        'n': {
            alias: 'frames',
            describe: 'number of frames to receive',
            type: 'number',
        },
    }).argv;

const workpad = {
    execDir: path.dirname(process.argv[1]),
    inQueue: [],            // input data queue, in an array of buffers
    messageStream: [],      // parsing from here messages
    remainingPayload: [],   // not completed frame inside a message
    parsing: false,         // message parsing is running
    timestamp: null,
    messageSeqno: null,
    frameCounter: 0,
    messagePrinter: (function useMessagePrinter(threshold) {
        var counter = 0;
        return (seqno, payload, frameCounter) => {
            if (++counter == threshold) {
                console.log('seq', seqno, 'payload len', payload.length,
                    'recved frames:', frameCounter);
                counter = 0;
            }
        };
    })(10),
    ws: null,
    outFilename: argv.out,
    csvInfo: null,
}

if (argv.out && ! argv.raw) {
    const { dir, name, ext } = path.parse(argv.out);
    workpad.csvInfo = {
        dir,
        name,
        ext,
        fileCounter: 0,
        rowsCounter: 0,
        header: 'Seqno,RecvTime,U1,I1,U2,I2,U3,I3',
    };
}

function writeCsvRow(row, workpad)
{
    const makeNextFilename = () => {
        return path.join(workpad.csvInfo.dir,
            [
                workpad.csvInfo.name, '-',
                (++workpad.csvInfo.fileCounter).toString().padStart(5, '0'),
                workpad.csvInfo.ext,
            ].join(''));
    };
    const newFile = () => {
        const filename = makeNextFilename();
        workpad.ws = fs.createWriteStream(filename);
        workpad.ws.write(workpad.csvInfo.header + '\n');
        workpad.csvInfo.rowCounter = 0;
        return filename;
    };
    const dispatchFile = filename => {
        const handlerFilename = path.join(workpad.execDir, FILE_HANDLER);
        console.log(`send ${filename} to ${handlerFilename}`);
        const cmdline = `${handlerFilename} -f ${filename}`;
        exec(cmdline, (error, stdout, stderr) => {
            if (error)
                throw new Error(`exec ${cmdline} error: ${error}`);
        });
    };

    if (! workpad.csvInfo) return;
    if (! workpad.ws)
        workpad.csvInfo.filename = newFile();
    if (workpad.csvInfo.rowCounter == MAX_ROWS_EACH_FILE) {
        workpad.ws.end();
        dispatchFile(workpad.csvInfo.filename);
        workpad.csvInfo.filename = newFile();
    }
    workpad.ws.write(row + '\n');
    ++workpad.csvInfo.rowCounter;
}

function writeRaw(data, workpad)
{
    if (! workpad.ws) {
        workpad.ws = workpad.outfFilename == '-' ? process.stdout
            : fs.createWriteStream(workpad.outFilename);
    }
    workpad.ws.write(data);
}

function endOutStream(workpad)
{
    if (workpad.ws) workpad.ws.end();
}

/**
 * Handle input data.
 * @data input data
 * @return true if end of handling, otherwise false.
 */
function handleInData(data)
{
    try {
        if (argv.raw) {
            writeRaw(data, workpad);
            return false;
        }
        putData(workpad, data);
        if (argv.frames && workpad.frameCounter >= argv.frames)
            return true;
    } catch (e) {
        endOutStream(workpad);
        throw e;
    }
    return false;
}

function useSocketClient()
{
    const client = new net.Socket();

    client.connect(argv.port, argv.host, () => {
        console.error('connected');
    });
    client.on('close', () => {
        endOutStream(workpad);
        console.error('connection closed');
    });
    client.on('data', data => {
        try {
            if (handleInData(data)) client.end();
        } catch (e) {
            client.end();
            throw(e);
        }
    });
    return client;
}

if (argv.host)
    useSocketClient();
else if (argv.input) {
    const rs = argv.input == '-' ? process.stdin
        : fs.createReadStream(argv.input)
    rs.on('close', () => {
        endOutStream(workpad);
    });
    rs.on('error', err => {
        endOutStream(workpad);
        throw(err);
    })
    rs.on('data', data => {
        if (handleInData(data)) rs.destroy();
    })
}
