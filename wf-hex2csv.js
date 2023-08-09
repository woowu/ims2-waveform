#!/usr/bin/node --harmony
'use strict';

/**
 * Convert intel-hex file which contains IMS2 waveform samples into csv, each
 * line of which represents value of a variable in the sample.
 */

const fs = require('fs');
const readline = require('readline');
const yargs = require('yargs/yargs');
const { hideBin } = require('yargs/helpers');

const argv = yargs(hideBin(process.argv))
    .option('input', {
        alias: 'i',
        describe: 'input waveform hex file',
        demandOption: true,
        nargs: 1,
    })
    .option('output', {
        alias: 'o',
        describe: 'output csv file',
        demandOption: true,
        nargs: 1,
    })
    .option('i1g', {
        describe: 'i1 gain',
        type: 'number',
    })
    .option('i2g', {
        describe: 'i2 gain',
        type: 'number',
    })
    .option('i3g', {
        describe: 'i3 gain',
        type: 'number',
    })
    .argv;

const scale = {
    u: .021522,
    i: .00461806,
};
var igain = [
    argv.i1g ? argv.i1g : 1,
    argv.i2g ? argv.i2g : 1,
    argv.i3g ? argv.i3g : 1,
];

const useStreamFSM = ws => {
    var skipCnt;
    var phase;
    var lo = 0;
    var state = head;
    var time = 0;

    const toSigned16 = value => {
        return value > 32767 ? -1 * (65536 - value) : value;
    };

    function head(byte) {
        if (byte != 0xa5)
            throw new Error('bad frame');
        phase = 0;
        state = uLow;
    }

    function uLow(byte) {
        lo = byte;
        state = uHigh;
    }

    function uHigh(byte) {
        const val = toSigned16(byte * 256 + lo) * scale.u;
        ws.write(`${time},u${phase + 1},${val}\n`);
        state = iLow;
    }

    function iLow(byte) {
        lo = byte;
        state = iHigh;
    };

    function iHigh(byte) {
        const val = toSigned16(byte * 256 + lo) * scale.i * igain[phase];
        ws.write(`${time},i${phase + 1},${val}\n`);
        if (++phase == 3) {
            skipCnt = 0;
            state = skip;
        } else
            state = uLow;
    };

    function skip(byte) {
        if (++skipCnt == 2) {
            state = head;
            time += 1E3/6400
        }
    };

    return byte => state(byte);
};

const ws = fs.createWriteStream(argv.output);
ws.write('time,name,value\n');
const fsm = useStreamFSM(ws);
const rl = readline.createInterface({
    input: fs.createReadStream(argv.input),
});
rl.on('line', line => {
    const pat = /^:10[0-9A-F]{4}00([0-9A-F]{32})/;
    var m;
    if (m = line.match(pat)) {
        var s = m[1];
        while (s.length) {
            fsm(parseInt(s.slice(0, 2), 16));
            s = s.slice(2);
        }
    }
});
