#!/usr/bin/node --harmony
'use strict';

/**
 * Send simulated waveform stream into a serial port, usually socat created
 * pseudo one, from which a client can receive the streaming.
 */

import { SerialPort } from 'serialport';
import yargs from 'yargs/yargs';
import fs from 'node:fs';

const S_FREQ = 6.4e3;

var sampleGenerator;

/**
 * singalFreq: AC singal frequency
 * sampleFreq: Sampling l frequency
 * uPeak: Peak voltages
 * iPeak: Peak currents
 * uk: voltage scaler
 * ik: current scaler
 * phaseShifts: phase shift of each of three lines in radians
 * csv: write stream for saving the sample data 
 */
function useSampleGenerator(format, signalFreq, sampleFreq
    , uPeak, iPeak, uk, ik, phaseShifts)
{
    return (n, csv) => {
        const payloadLen = 12;
        const frame = [0xa5];
        var cs0 = 0, cs1 = 0;
        const sample = [];
        for (var line = 0; line < 3; ++line) {
            const freqComp = 2*Math.PI * signalFreq/sampleFreq * n;
            const lineDelay = line * 2 * Math.PI / 3;
            const u = uPeak[line] * Math.cos(
                freqComp + lineDelay
            );
            const i = iPeak[line] * Math.cos(
                freqComp + lineDelay - phaseShifts[line]
            );
            const uScaled = Math.round(u * uk);
            const iScaled = Math.round(i * ik);

            var b0, b1;
            const view = new DataView(new ArrayBuffer(2));

            view.setInt16(0, uScaled, true);
            b0 = view.getUint8(0);
            b1 = view.getUint8(1);
            frame.push(b0);
            frame.push(b1);
            cs0 ^= b0;
            cs1 ^= b1;

            view.setInt16(0, iScaled, true);
            b0 = view.getUint8(0);
            b1 = view.getUint8(1);
            frame.push(b0);
            frame.push(b1);
            cs0 ^= b0;
            cs1 ^= b1;

            sample.push({
                u,
                uScaled,
                i,
                iScaled,
            });
        }
        if (csv)
            csv.write(`${n},`
                + `${sample[0].u.toFixed(3)},`
                + `${sample[0].i.toFixed(3)},`
                + `${sample[1].u.toFixed(3)},`
                + `${sample[1].i.toFixed(3)},`
                + `${sample[2].u.toFixed(3)},`
                + `${sample[2].i.toFixed(3)},`
                + `${sample[0].uScaled},`
                + `${sample[0].iScaled},`
                + `${sample[1].uScaled},`
                + `${sample[1].iScaled},`
                + `${sample[2].uScaled},`
                + `${sample[2].iScaled}`
                + '\n'
            );
        if (format == 'au') {
            frame.push(cs0);
            frame.push(cs1);
        }
        return frame;
    };
}

function generateSamples(nStart, nEnd, csv)
{
    var bytes = [];
    for (var i = nStart; i < nEnd; ++i)
        bytes = [...bytes, ...sampleGenerator(i, csv)];
    return bytes;
}

function wrLoop(serial, max_samples, BER, csv)
{
    const period = 25;
    const speedGain = 1.65;
    const step = parseInt(period * 1e-3 * S_FREQ * speedGain);
    var n = 0;
    var stat_cnt = 0;
    var t;
    var octetsSent = 0;

    t = new Date().valueOf();
    (function next() {
        var end = n + step;
        if (max_samples > 0 && end > max_samples)
            end = max_samples;
        const data = generateSamples(n, end, csv);
        octetsSent = data.length;
        if (BER && octetsSent >= BER) {
            const idx1 = parseInt(Math.random() * data.length);
            const idx2 = parseInt(Math.random() * 8);
            data[idx1] ^= 1 << idx2;
            octetsSent = 0;
        }
        serial.write(data);

        if (end == max_samples) {
            csv.end();
            serial.end();
            return;
        }

        ++stat_cnt;
        if (stat_cnt == 100) {
            const duration = new Date().valueOf() - t;
            const size = stat_cnt * step * 15;
            const kbps = (size * 10)/duration;
            console.log(`sent ${size} octets, ${kbps.toFixed(3)} kbps`);

            t = new Date().valueOf();
            stat_cnt = 0;
        }

        n += step;
        setTimeout(next, period);
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
        'n': {
            alias: 'number',
            describe: 'number of data samples',
            type: 'number',
            default: 0,
        },
        'B': {
            alias: 'BER',
            describe: 'after how many bytes, there will be a one bit error',
            type: 'number',
            default: 0,
        },
        'P': {
            alias: 'phi',
            describe: 'Phase shift of each line in comma separated degrees, e.g., 0,-15,20',
            type: 'string',
            default: '0,0,0',
        },
        'S': {
            alias: 'scaler',
            describe: 'Inverse scalers for voltage and current, separated by comma',
            type: 'string',
            default: '2.1522e-2,4.61806e-3',
        },
        'V': {
            alias: 'uPeak',
            describe: 'Peak voltages of each line in comma separated volts',
            type: 'number',
            default: '311,311,311',
        },
        'A': {
            alias: 'iPeak',
            describe: 'Peak currents of each line in comma separated amps',
            type: 'number',
            default: '15,15,15',
        },
        'c': {
            alias: 'csv',
            describe: 'Csv file to save the generated sample data',
            type: 'string',
        },
    }).argv;

if (argv.number < 0) {
    console.error("Number of data samples cannot be negative.");
    process.exit(1);
}
if (argv.BER < 0) {
    console.error("BER must be a non-negative integer.");
    process.exit(1);
}
var phi = argv.phi.split(',');
if (phi.length != 3) {
    console.error('phase shifts need to be three degrees');
    process.exit(1);
}
phi = phi.map(x => parseFloat(x)/180 * Math.PI);

var scaler = argv.scaler.split(',');
if (scaler.length != 2) {
    console.error('Need two scalers');
    process.exit(1);
}
scaler = scaler.map(x => 1/parseFloat(x));

var uPeak = argv.uPeak.split(',');
var iPeak = argv.iPeak.split(',');
if (uPeak.length != 3) {
    console.error('There are three V\'s needed');
    process.exit(1);
}
uPeak = uPeak.map(x => parseFloat(x));
if (iPeak.length != 3) {
    console.error('There are three A\'s needed');
    process.exit(1);
}
iPeak = iPeak.map(x => parseFloat(x));

var ws = null;
if (argv.csv) {
    ws = fs.createWriteStream(argv.csv);
    ws.write('Seqno,u1,i1,u2,i2,u3,i3,'
        + 'u1Scaled,i1Scaled,u2Scaled,i2,i2Scaled,u3Scaled,i3Scaled\n');
}
sampleGenerator = useSampleGenerator(argv.format, 50, S_FREQ
    , uPeak, iPeak
    , scaler[0], scaler[1]
    , phi);

const seri = new SerialPort({
    path: argv.device,
    baudRate: argv.baud,
    autoOpen: false,
});

seri.open(err => {
    if (err) throw new Error(err);
    wrLoop(seri, argv.number, argv.BER, ws);
    ws.on('close', () => {
        process.exit(0);
    });
});
seri.on('data', () => {
});
