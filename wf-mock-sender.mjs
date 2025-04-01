#!/usr/bin/node --harmony
'use strict';

/**
 * Send simulated waveform stream into a serial port, usually socat created
 * pseudo one, from which a client can receive the streaming.
 */

import { SerialPort } from 'serialport';
import yargs from 'yargs/yargs';

const S_FREQ = 6.4e3;

var sampleGenerator;

/**
 * singalFreq: AC singal frequency
 * sampleFreq: Sampling l frequency
 * vPeak: Peak voltage
 * aPeak: Peak current
 * vScaler: voltage scaler
 * aScaler: current scaler
 */
function useSampleGenerator(format, signalFreq, sampleFreq
    , vPeak, aPeak, vScaler, aScaler)
{
    return n => {
        const payloadLen = 12;
        const frame = [0xa5];
        var cs0 = 0, cs1 = 0;
        for (var line = 0; line < 3; ++line) {
            const v = Math.round(vPeak * Math.cos(
                2*Math.PI * signalFreq/sampleFreq * n + line * 2 * Math.PI / 3
            ) * vScaler);
            const a = Math.round(aPeak * Math.cos(
                2*Math.PI * signalFreq/sampleFreq * n + line * 2 * Math.PI / 3
            ) * aScaler);

            var b0, b1;
            const view = new DataView(new ArrayBuffer(2));

            view.setInt16(0, v, true);
            b0 = view.getUint8(0);
            b1 = view.getUint8(1);
            frame.push(b0);
            frame.push(b1);
            cs0 ^= b0;
            cs1 ^= b1;

            view.setInt16(0, a, true);
            b0 = view.getUint8(0);
            b1 = view.getUint8(1);
            frame.push(b0);
            frame.push(b1);
            cs0 ^= b0;
            cs1 ^= b1;
        }
        if (format == 'au') {
            frame.push(cs0);
            frame.push(cs1);
        }
        return frame;
    };
}

function generateSamples(nStart, nEnd)
{
    var bytes = [];
    for (var i = nStart; i < nEnd; ++i)
        bytes = [...bytes, ...sampleGenerator(i)];
    return bytes;
}

function wrLoop(stream)
{
    const period = 25;
    const speedAdj = 80;
    const step = period * 1e-3 * S_FREQ + speedAdj;
    var n = 0;
    var cnt = 0;
    var t;

    t = new Date().valueOf();
    (function next() {
        stream.write(generateSamples(n, n + step));
        n += step;
        setTimeout(next, period);

        ++cnt;
        if (cnt == 50) {
            const duration = new Date().valueOf() - t;
            const size = cnt * step * 15;
            const kbps = (size * 10)/duration;
            console.log(`sent ${size} octets, ${kbps.toFixed(3)} kbps`);

            t = new Date().valueOf();
            cnt = 0;
        }
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
            default: 1333300,
            type: 'number',
        },
        'm': {
            alias: 'format',
            describe: 'frame format. Currently supported: au, usb',
            type: 'string',
            default: 'au',
        },
    }).argv;

sampleGenerator = useSampleGenerator(argv.format, 50, S_FREQ
    , 220*Math.sqrt(2), 10*Math.sqrt(2)
    , 1/2.1522e-2, 1/4.61806e-3);

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
