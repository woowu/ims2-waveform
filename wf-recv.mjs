#!/usr/bin/node

/* Receive waveform streaming from TCP. Each message
 * is in the form:
 *   0x3e | seqno | 64bit timestamp | 32 bit len | waveform chunk
 */

import fs from 'node:fs';
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

function putData(workpad, data)
{
    workpad.instream = [...workpad.instream, ...data];
}

function parseStream(workpad, ws)
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
            console.log('bad frame: ', frameCounter);
            return false;
        }

        if (! ws) return true;
        ws.write(`${frameCounter * (1/WF_SAMPLE_RATE)},`
            + `${moment(workpad.timestamp).format('YYYY-MM-DD HH:mm:ss.SSS')}`);
        var pos = WF_FRAME_HEAD_LEN;
        var value;
        for (var channel = 0; channel < 3; ++channel) {
            for (var q = 0; q < 2; ++q) {
                value = new DataView(new Uint8Array(
                    frame.slice(pos, pos + 2)
                ).buffer).getInt16(0, true);
                ws.write(',' + value);
                pos += 2;
            }
        }
        ws.write('\n');
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
            if (data[pos] == WF_FRAME_SYNC && handleFrame(
                data.slice(pos, pos + WF_FRAME_LEN), workpad.frameCounter)) {
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

    /* parse and strip a whole message, otherwise keep the stream
     * data untouched.
     */

    if (workpad.instream.length < MESSAGE_HEAD_LEN) return;
    if (workpad.instream[0] != MESSAGE_START) throw new Error('bad message');
    const messageSeqno = new DataView(
        new Uint8Array(workpad.instream.slice(
            MESSAGE_START_LEN, MESSAGE_START_LEN + MESSAGE_SEQNO_LEN)).buffer)
        .getUint32(0)
    const payloadLen = new DataView(
        new Uint8Array(workpad.instream.slice(
            MESSAGE_START_LEN + MESSAGE_SEQNO_LEN + MESSAGE_TIMESTAMP_LEN
            , MESSAGE_START_LEN + MESSAGE_SEQNO_LEN + MESSAGE_TIMESTAMP_LEN + MESSAGE_LEN_SZ)).buffer)
        .getUint32(0)
    if (workpad.instream.length < MESSAGE_HEAD_LEN + payloadLen) return;

    if (workpad.messageSeqno === null) workpad.messageSeqno = messageSeqno;
    if (messageSeqno != workpad.messageSeqno)
        throw new Error(`incorrect message seqno. received ${messageSeqno}` +
            ` expected ${workpad.messageSeqno}`)
    workpad.timestamp = new Date(Number(new DataView(
        new Uint8Array(workpad.instream.slice(MESSAGE_START_LEN + MESSAGE_SEQNO_LEN
            , MESSAGE_START_LEN + MESSAGE_SEQNO_LEN + MESSAGE_TIMESTAMP_LEN)).buffer)
        .getBigInt64()))
    const payload = workpad.instream.slice(MESSAGE_HEAD_LEN, MESSAGE_HEAD_LEN + payloadLen);
    workpad.messagePrinter(messageSeqno, payload, workpad.frameCounter);
    workpad.instream = workpad.instream.slice(MESSAGE_HEAD_LEN + payloadLen);
    ++workpad.messageSeqno;
    parsePayload(payload);
}

const argv = yargs(process.argv.slice(2))
    .option({
        'h': {
            alias: 'host',
            demandOption: true,
            describe: 'remote host',
            type: 'string',
        },
        'p': {
            alias: 'port',
            demandOption: true,
            describe: 'remote tcp port',
            type: 'number',
        },
        'c': {
            alias: 'csv',
            describe: 'csv output filename',
            type: 'string',
        },
        'n': {
            alias: 'frames',
            describe: 'number of frames to receive',
            type: 'number',
        },
    }).argv;

const workpad = {
    instream: [],
    timestamp: null,
    remainingPayload: [],
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
}
const client = new net.Socket();
var recvCnt = 0;
var ws = null;
if (argv.csv) {
    ws = fs.createWriteStream(argv.csv);
    ws.write('Time,RecvTime,U1,I1,U2,I2,U3,I3\n');
}

client.connect(argv.port, argv.host, () => {
    console.log('connected');
});
client.on('close', () => {
    if (ws) ws.end();
    console.log('connection closed');
});
client.on('data', data => {
    try {
        putData(workpad, data);
        parseStream(workpad, ws);
        if (argv.frames && workpad.frameCounter >= argv.frames)
            client.end();
    } catch (e) {
        if (ws) ws.end();
        client.end();
        throw e;
    }
});
