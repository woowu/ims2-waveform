#!/usr/bin/node

/**
 * When receive waveform streaming from TCP. Each message
 * is in the form:
 *   0x3e | seqno | 64bit timestamp | 32 bit len | waveform chunk
 * The waveform chunk contains one or more or incompleted frames as
 * described below.
 *
 * When receive waveform streaming from serial port. and the frame format is
 * 'au', the stream contains frames of the form:
 *   0xa5 | A_l1 | V_l1 | A_l2 | V_L2 | A_l3 | V_l3 | chksum 
 * All A/V quantities are 16-bit integer in little endian. Checksum is 16-bit.
 *
 * When receive waveform streaming from serial port. and the frame format is
 * 'usb', the stream contains frames of the form:
 *   0xa5 | A_l1 | V_l1 | A_l2 | V_L2 | A_l3 | V_l3
 * All A/V quantities are 16-bit integer in little endian. Checksum is 16-bit.
 */

import path from 'node:path';
import fs from 'node:fs';
import { exec } from 'node:child_process';
import net from 'node:net';
import yargs from 'yargs/yargs';
import dump from 'buffer-hexdump';
import moment from 'moment';
import { SerialPort } from 'serialport';

const WF_SAMPLE_RATE = 6.4e3;
const WF_FRAME_SYNC = 0xa5;
const WF_FRAME_HEAD_LEN = 1;
const WF_FRAME_PAYLOAD_LEN = (2 + 2) * 3; /* u/i from 3 channels */
const WF_FRAME_CS_LEN = 2;
const MESSAGE_START = 0x3e;
const MESSAGE_START_LEN = 1;
const MESSAGE_SEQNO_LEN = 4;
const MESSAGE_TIMESTAMP_LEN = 8;
const MESSAGE_LEN_SZ = 4;
const MESSAGE_HEAD_LEN = MESSAGE_START_LEN + MESSAGE_SEQNO_LEN + MESSAGE_TIMESTAMP_LEN + MESSAGE_LEN_SZ;
const MAX_ROWS_EACH_FILE = 5.76e6;
const FILE_HANDLER = 'wf-overview.R';

const workpad = {
    execDir: path.dirname(process.argv[1]),
    ws: null,
    outFilename: null,
    dispatch: false,
    frameInMessage: true,   /* frames are wrappered in messages */
    frameFormat: 'au',
};

var csvInfo = null;

const inQueue = [];         // input data queue, in an array of buffers

const inStream = {
    buf: [],                // bytes stream which formed by messages.
    remainingPayload: [],   // not completed frame inside a message
    nextSeqno: null,        // expected seqno of the next message
    frameCounter: 0,
    timestamp: null,

    messagePrinter: (function useMessagePrinter(threshold) {
        var counter = 0;
        var len = 0;
        return (seqno, payload, frameCounter) => {
            if (++counter == threshold) {
                console.log(`message seq ${seqno} len ${payload.length}`
                    + ` data used ${frameCounter * (WF_FRAME_HEAD_LEN + WF_FRAME_PAYLOAD_LEN)}`);
                counter = 0;
                len = 0;
            } else {
                len += payload.length;
            }
        };
    })(10),
};

const streamPerf = {
    lastLen: 0,
    lastTime: new Date().getTime(),
    updateCounter: 0,
};

function putData(workpad, inQueue, data)
{
    inQueue.push(data);
    if (inQueue.length == 1)
        parseStream(inQueue, inStream, workpad);
}

function waitDataDrain() {
    if (! inQueue.length) {
        /* it's possible there are still works in the inStream, but I
         * cannot simply check the length of the stream because it can contains
         * incompleted message while the next chunk of data will never come.
         */
        setTimeout(() => {
            endOutStream(workpad);
            process.exit();
        }, 1000);
        return;
    }
    setTimeout(() => {
        console.log('waiting data drain');
        waitDataDrain();
    }, 1000);
}

function handleFrame(frame, frameCounter, timestamp, workpad)
{
    if (workpad.frameFormat == 'au') {
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
    }

    var line = `${frameCounter},`
        + `${moment(timestamp).format('YYYY-MM-DD HH:mm:ss.SSS')}`;
    var pos = WF_FRAME_HEAD_LEN;
    var value;
    for (var channel = 0; channel < 3; ++channel) {
        for (var q = 0; q < 2; ++q) {
            value = new DataView(new Uint8Array(
                frame.slice(pos, pos + 2)
            ).buffer).getInt16(0, workpad.frameFormat == 'au');
            line += ',' + value;
            pos += 2;
        }
    }
    writeCsvRow(line, workpad);
    return true;
}

function parsePayload(payload, inStream, workpad)
{
    const data = [...inStream.remainingPayload, ...payload];

    /* Only try to parse whole frames, for the last incompleted frame,
     * leave it for the next message.
     */
    var last_successful_pos = 0;
    var pos = 0;
    var frameLen = WF_FRAME_HEAD_LEN + WF_FRAME_PAYLOAD_LEN
        + (workpad.frameFormat == 'au' ? 2 : 0);

    while (data.length - pos >= frameLen) {
        if (data[pos] == WF_FRAME_SYNC
                && handleFrame(data.slice(pos, pos + frameLen),
                    pos == last_successful_pos
                        ? inStream.frameCounter
                        : inStream.frameCounter + 1,
                    inStream.timestamp, workpad)) {
            if (pos > last_successful_pos) {
                console.log(`skipped ${pos - last_successful_pos} bytes`);
                /* all the continuous not-consumed bytes are counted as a
                 * single bad frame, which should occupy a counter.
                 */
                ++inStream.frameCounter;
            }
            pos += frameLen;
            last_successful_pos = pos;
            ++inStream.frameCounter;
        } else {
            ++pos;
        }
    }
    inStream.remainingPayload = data.slice(last_successful_pos);
}

function parseStream(inQueue, inStream, workpad)
{
    /**
     * @payload payload of a single message which contains N frames, the last
     *   one may be incompleted.
     */
    const tryNext = () => {
        if (inQueue.length)
            setImmediate(() => {
                parseStream(inQueue, inStream, workpad);
            });
    };

    const parseMessages = () => {
        while (inStream.buf.length >= MESSAGE_HEAD_LEN) {
            if (inStream.buf[0] != MESSAGE_START) {
                console.error(dump(inStream.buf).slice(0, 64));
                throw new Error('bad message');
            }
            const messageSeqno = new DataView(
                new Uint8Array(inStream.buf.slice(
                    MESSAGE_START_LEN, MESSAGE_START_LEN + MESSAGE_SEQNO_LEN)).buffer)
                .getUint32(0)
            const payloadLen = new DataView(
                new Uint8Array(inStream.buf.slice(
                    MESSAGE_START_LEN + MESSAGE_SEQNO_LEN + MESSAGE_TIMESTAMP_LEN
                    , MESSAGE_START_LEN + MESSAGE_SEQNO_LEN
                        + MESSAGE_TIMESTAMP_LEN + MESSAGE_LEN_SZ)).buffer)
                .getUint32(0)
            if (inStream.buf.length < MESSAGE_HEAD_LEN + payloadLen)
                break;

            if (inStream.nextSeqno === null)
                inStream.nextSeqno = messageSeqno;
            if (messageSeqno != inStream.nextSeqno)
                throw new Error(`incorrect message seqno. received ${messageSeqno}` +
                    ` expected ${inStream.nextSeqno}`)
            inStream.timestamp = new Date(Number(new DataView(
                new Uint8Array(inStream.buf.slice(
                    MESSAGE_START_LEN + MESSAGE_SEQNO_LEN,
                    MESSAGE_START_LEN + MESSAGE_SEQNO_LEN + MESSAGE_TIMESTAMP_LEN)).buffer)
                .getBigInt64()))
            const payload = inStream.buf.slice(
                MESSAGE_HEAD_LEN, MESSAGE_HEAD_LEN + payloadLen);
            inStream.messagePrinter(messageSeqno, payload,
                inStream.frameCounter);
            inStream.buf = inStream.buf.slice(
                MESSAGE_HEAD_LEN + payloadLen);
            ++inStream.nextSeqno;
            parsePayload(payload, inStream, workpad);
        }
    };

    /* Array has a size limit equals to 2^32 - 2, here we keep the water mark
     * as half height as the limit.
     */
    if (! inQueue[0] || inQueue[0].length + inStream.buf.length > 2**32/2) {
        tryNext();
        return;
    }

    const b = inQueue.shift();

    updateStreamPerf(streamPerf, inStream.buf.length, b.length);
    if (! (streamPerf.updateCounter % 10))
        console.log(`new chunk len: ${b.length} `
            + `buf: ${streamPerf.lastLen} queue: ${inQueue.length} `
            + ` consuming speed: ${streamPerf.kbps.toFixed(3)} kpbs`);
    inStream.buf.push(...b);

    /* Parse and strip whole messages, incompleted message will be left in
     * the stream buf intouched.
     */
    if (workpad.frameInMessage)
        parseMessages();
    else {
        const d = [...inStream.buf];
        inStream.buf = [];
        inStream.timestamp = new Date();
        parsePayload(d, inStream, workpad);
    }

    tryNext();
}

function updateStreamPerf(perf, currLen, appendLen)
{
    const consumed = perf.lastLen - currLen;
    perf.kbps = consumed * 8 / (new Date().getTime() - perf.lastTime);

    perf.lastLen = currLen + appendLen;
    perf.lastTime = new Date().getTime();
    ++perf.updateCounter;
}

function writeCsvRow(row, workpad)
{
    const makeNextFilename = () => {
        return path.join(csvInfo.dir,
            [
                csvInfo.name, '-',
                (++csvInfo.fileCounter).toString().padStart(5, '0'),
                csvInfo.ext,
            ].join(''));
    };
    const newFile = () => {
        const filename = makeNextFilename();
        workpad.ws = fs.createWriteStream(filename);
        workpad.ws.write(csvInfo.header + '\n');
        csvInfo.rowCounter = 0;
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

    if (! csvInfo) return;
    if (! workpad.ws)
        csvInfo.filename = newFile();
    if (csvInfo.rowCounter == MAX_ROWS_EACH_FILE) {
        endOutStream(workpad);
        if (workpad.dispatch) dispatchFile(csvInfo.filename);
        csvInfo.filename = newFile();
    }
    workpad.ws.write(row + '\n');
    ++csvInfo.rowCounter;
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
        putData(workpad, inQueue, data);
        if (argv.frames && inStream.frameCounter >= argv.frames)
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
    client.on('error', e => {
        throw(e); 
    });
    client.on('close', () => {
        console.error('connection closed');
        waitDataDrain();
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

function useSerialPort({ device, baud })
{
    const seri = new SerialPort({
        path: device,
        baudRate: baud,
        autoOpen: false,
    });

    seri.open(err => {
        if (err)
            throw new Error(err);
    });
    seri.on('data', data => {
        if (handleInData(data))
            throw new Error('serial port reading error');
    });
}

/*---------------------------------------------------------------------------*/

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
        'd': {
            alias: 'device',
            describe: 'receive from this serial device',
            type: 'string',
        },
        'b': {
            alias: 'baud',
            describe: 'baud rate',
            default: 921600,
            type: 'number',
        },
        'n': {
            alias: 'frames',
            describe: 'number of frames to receive',
            type: 'number',
        },
        'm': {
            alias: 'format',
            describe: 'frame format. Currently supported: au, usb',
            type: 'string',
            default: 'au',
        },
        'c': {
            alias: 'dispatch',
            describe: 'dispatch generated csv for post processing',
            type: 'boolean',
        },
    }).argv;

workpad.outFilename = argv.out;
workpad.dispatch = argv.dispatch;

if (argv.out && ! argv.raw) {
    const { dir, name, ext } = path.parse(argv.out);
    csvInfo = {
        dir,
        name,
        ext,
        fileCounter: 0,
        rowsCounter: 0,
        header: 'Seqno,RecvTime,U1,I1,U2,I2,U3,I3',
    };
}

if (argv.format != 'au' && argv.format != 'usb') {
    console.error('unknown frame format');
    process.exit(1);
}
workpad.frameFormat = argv.format;

if (argv.host)
    useSocketClient();
else if (argv.device) {
    workpad.frameInMessage = false;
    useSerialPort({ device: argv.device, baud: argv.baud });
} else if (argv.input) {
    const rs = argv.input == '-' ? process.stdin
        : fs.createReadStream(argv.input)
    rs.on('close', () => {
        waitDataDrain();
    });
    rs.on('error', err => {
        endOutStream(workpad);
        throw(err);
    })
    rs.on('data', data => {
        if (handleInData(data)) rs.destroy();
    })
}
