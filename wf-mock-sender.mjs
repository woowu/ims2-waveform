#!/usr/bin/node --harmony
'use strict';

# Send simulated waveform stream into a serial port, usually socat created
# pseudo one, from which a client can receive the streaming.

import { SerialPort } from 'serialport';
import yargs from 'yargs/yargs';

var frameGenerator;

function useFrameGenerator(format)
{
    return () => {
        const payloadLen = 12;
        const frame = [0xa5];
        var cs = [0, 0];
        for (var i = 0; i < payloadLen; ++i) {
            const b = Math.floor(256 * Math.random())
            if (format == 'au') cs[i % 2] ^= b; 
            frame.push(b);
        }
        if (format == 'au') {
            frame.push(cs[0]);
            frame.push(cs[1]);
        }
        return frame;
    };
}

function generateNFrames(n)
{
    var bytes = [];
    for (var i = 0; i < n; ++i)
        bytes = [...bytes, ...frameGenerator()];
    return bytes;
}

function wrLoop(stream)
{
    (function next() {
        stream.write(generateNFrames(128 * 10));
        setTimeout(next, 1);
    }());
}

/*---------------------------------------------------------------------------*/

const argv = yargs(process.argv.slice(2))
    .option({
        'd': {
            alias: 'device',
            describe: 'serial device',
            type: 'string',
            demandOption: true,
        },
        'b': {
            alias: 'baud',
            describe: 'baud rate',
            default: 921600,
            type: 'number',
        },
        'm': {
            alias: 'format',
            describe: 'frame format. Currently supported: au, usb',
            type: 'string',
            default: 'au',
        },
    }).argv;

frameGenerator = useFrameGenerator(argv.format);

const seri = new SerialPort({
    path: argv.device,
    baudRate: argv.baud,
    autoOpen: false,
});

seri.open(err => {
    if (err) throw new Error(err);
});
seri.on('data', () => {
});
wrLoop(seri);
