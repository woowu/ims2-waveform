#!/usr/bin/node --harmony
'use strict';

import net from 'node:net';

const TCP_PORT = 4059;

function generateRandomFrame()
{
    const payloadLen = 12;
    const frame = [0xa5];
    var cs = [0, 0];
    for (var i = 0; i < payloadLen; ++i) {
        const b = Math.floor(256 * Math.random())
        cs[i % 2] ^= b; 
        frame.push(b);
    }
    frame.push(cs[0]);
    frame.push(cs[1]);
    return frame;
}

function generateNFrames(n)
{
    var bytes = [];
    for (var i = 0; i < n; ++i)
        bytes = [...bytes, ...generateRandomFrame()];
    return bytes;
}

function handleConnection(socket)
{
    var blockSeqno = 0;
    const BLOCK_HEAD = 0x3e;
    const BLOCK_MAX_PAYLOAD_LEN = 3000;
    const BLOCK_MIN_PAYLOAD_LEN = 100;
    var end = false;
    var streamNoErrLen = 0;
    const errorRate = 1e5;

    socket.on('end', () => {
        end = true;
        console.log('disconnected');
    });
    socket.on('error', err => {
        end = true;
        console.error('error:', err.message);
    });

    (function nextStream() {
        var stream = generateNFrames(128 * 10);
        streamNoErrLen += stream.length;
        if (streamNoErrLen >= errorRate) {
            const pos = Math.floor(stream.length * Math.random());
            stream[pos] = ~stream[pos];
            streamNoErrLen = 0;
        }
        console.log('genretated stream. len', stream.length);
        while (stream.length && ! end) {
            var blockPayloadLen;
            do {
                blockPayloadLen = Math.floor(BLOCK_MAX_PAYLOAD_LEN * Math.random());
            } while (blockPayloadLen < BLOCK_MIN_PAYLOAD_LEN);

            const payload = stream.slice(0, blockPayloadLen);
            stream = stream.slice(blockPayloadLen);

            const t = new Date().getTime();
            const ab = new ArrayBuffer(1 + 4 + 8 + 4);
            new DataView(ab).setUint8(0, BLOCK_HEAD);
            new DataView(ab).setUint32(1, ++blockSeqno);
            new DataView(ab).setBigInt64(1 + 4, BigInt(t));
            new DataView(ab).setUint32(1 + 4 + 8, payload.length);
            const block = Buffer.concat([Buffer.from(ab), Buffer.from(payload)]);
            console.log('send block. size', block.length, block.slice(0, 20), '...');
            socket.write(block);
        }
        if (! end) setTimeout(nextStream, 1);
    }());
}

const server = net.createServer(socket => {
    console.log('new connection from', `${socket.remoteAddress}:${socket.remotePort}`);
    setImmediate(() => {
        try {
            handleConnection(socket);
        } catch(e) {
            socket.end();
            throw(e);
        }
    });
});
server.on('error', err => {
    console.error('error:', err.message);
});
server.listen(TCP_PORT, () => {
    console.log('listening on port', TCP_PORT);
});
