#!/usr/bin/node

/* Receive waveform streaming from TCP. Each message
 * is in the form:
 *   64bit timestamp | 32 bit len | waveform chunk
 */

import fs from 'node:fs';
import net from 'node:net';
import yargs from 'yargs/yargs';
import dump from 'buffer-hexdump';
import moment from 'moment';

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

/* copy from E355 datamodel */
const ScalingFactors = [
    6.72565e-04, // UScaleInst
    1.44314e-04, // IScaleInst
];

function putData(workpad, data)
{
    workpad.instream = [...workpad.instream, ...data];
}

function parseStream(workpad, ws)
{
    const handleFrame = (frame, frameCount) => {
        console.log(frameCount + ':', dump(frame).slice(10, 10 + 2*15 + 7));
        var cs = [0, 0];
        var i = 0;
        for (const b of frame.slice(WF_FRAME_HEAD_LEN
            , WF_FRAME_HEAD_LEN + WF_FRAME_PAYLOAD_LEN)) {
            cs[i % 2] ^= b;
            ++i;
        }
        if (cs[0] != frame[WF_FRAME_HEAD_LEN + WF_FRAME_PAYLOAD_LEN]
            || cs[1] != frame[WF_FRAME_HEAD_LEN + WF_FRAME_PAYLOAD_LEN + 1]) {
            console.log('bad frame: ', frameCount);
            return;
        }

        if (! ws) return;
        ws.write(`${frameCount},${moment(workpad.timestamp).format('YYYY-MM-DD HH:mm:ss.SSS')}`);
        var pos = WF_FRAME_HEAD_LEN;
        var value;
        for (var channel = 0; channel < 3; ++channel) {
            for (var q = 0; q < 2; ++q) {
                value = new DataView(new Uint8Array(
                    frame.slice(pos, pos + 2)
                ).buffer).getInt16(0, true);
                ws.write(',' + value * ScalingFactors[q]);
                pos += 2;
            }
        }
        ws.write('\n');
    };

    const parsePayload = payload => {
        console.log(moment(workpad.timestamp).format('YYYY-MM-DD HH:mm:ss.SSS'));

        const data = [...workpad.remainingPayload, ...payload];
        var pos = 0;

        /* try sync to the next frame head if we have some lost
         * in the raw waveform streaming data.
         */
        while (data[pos] != WF_FRAME_SYNC) ++pos;
        if (pos) console.warn(`skipped ${pos} bytes in stream`);

        while (data.length - pos >= WF_FRAME_LEN) {
            handleFrame(data.slice(pos, pos + WF_FRAME_LEN)
                , workpad.frameCount++);
            pos += WF_FRAME_LEN;
        }
        workpad.remainingPayload = data.slice(pos);
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

    console.log('seq', messageSeqno, 'payload len', payloadLen);
    workpad.timestamp = new Date(Number(new DataView(
        new Uint8Array(workpad.instream.slice(MESSAGE_START_LEN + MESSAGE_SEQNO_LEN
            , MESSAGE_START_LEN + MESSAGE_SEQNO_LEN + MESSAGE_TIMESTAMP_LEN)).buffer)
        .getBigInt64()))
    const payload = workpad.instream.slice(MESSAGE_HEAD_LEN, MESSAGE_HEAD_LEN + payloadLen);
    workpad.instream = workpad.instream.slice(MESSAGE_HEAD_LEN + payloadLen);
    ++workpad.messageCount;
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
    messageCount: 0,
    frameCount: 0,
}
const client = new net.Socket();
var recvCnt = 0;
var ws = null;
if (argv.csv)
    ws = fs.createWriteStream(argv.csv);

client.connect(argv.port, argv.host, () => {
    console.log('connected');
});
client.on('close', () => {
    if (ws) ws.end();
    console.log('connection closed');
});
client.on('data', data => {
    putData(workpad, data);
    parseStream(workpad, ws);
    if (argv.frames && workpad.frameCount >= argv.frames) {
        client.end();
        return;
    }
});
