# Receive Picasso IMS2 A/V signal waveform

## Installation

To install ims-waveform tool, you need to firstly install nodejs on your Linux or Window. Then change into the uncompressed directory of the ims-waveform followed by running the below command to install all the dependencies:

```
npm i
```

## Enable IMS waveform streaming

Using a meter TestApp firmware and run the below SCPI command to enable the waveform streaming:

```
IMS:WaveForm:ON
```

Power cycle the meter after issued the command.

## Receive waveforms over the serial port

Connect your PC, using an USB-to-TTL cable, to the meter waveform streaming UART pins (PIN 11 and PIN 1) following the below diagram:

![Serial connection](./doc/serial-connection.svg)

Run wf-recv on Linux PC to receive the waveforms:
```
./wf-recv.mjs --device /dev/ttyUSB0 --baud 1333000 --format au --out myfile.csv

```

Run wf-recv on Windows PC to receive the waveforms:
```
node wf-recv.mjs --device COM5 --baud 1333000 --format au --out myfile.csv

```

The output csv file will be split into multiple files: myfile-0001.csv,
myfile-0002.csv, and so on.

Frame format:

```
  0xa5 | A_l1 | V_l1 | A_l2 | V_L2 | A_l3 | V_l3 | chksum 
```

All A/V quantities are 16-bit integer in little endian. Checksum is 16-bit.


## Receive waveforms over USB

Connect your PC and the meter using an USB to serial cable following the below diagram:

![USB connection](./doc/usb-connection.svg)

Run wf-recv on Linux PC to receive the waveforms:
```
./wf-recv.mjs --device /dev/ttyUSB0 --format usb --out myfile.csv

```

Run wf-recv on Windows PC to receive the waveforms:
```
node wf-recv.mjs --device COM5 --format usb --out myfile.csv

```

The output csv file will be split into multiple files: myfile-0001.csv,
myfile-0002.csv, and so on.

Frame format:

```
  0xa5 | A_l1 | V_l1 | A_l2 | V_L2 | A_l3 | V_l3
```

All A/V quantities are 16-bit integer in big endian.

## Inspect the screen output of wf-recv

In additional to write received waveform information to CSV files, the wf-recv tool also print performance and diagnostic information while running. Below is a sample output of the screen output:

```
12953 frame 168389 bytes 669.539 kbps avg 0.000 kbps FER NaN%
13016 frame 169208 bytes 665.846 kbps avg 667.879 kbps FER 0.00%
12811 frame 166543 bytes 666.172 kbps avg 666.858 kbps FER 0.00%
12749 frame 165737 bytes 662.617 kbps avg 666.631 kbps FER 0.00%
13120 frame 170560 bytes 666.575 kbps avg 665.551 kbps FER 0.00%
13144 frame 170872 bytes 664.872 kbps avg 665.759 kbps FER 0.00%
13032 frame 169416 bytes 665.028 kbps avg 665.609 kbps FER 0.00%
13088 frame 170144 bytes 665.275 kbps avg 665.478 kbps FER 0.00%
13048 frame 169624 bytes 665.192 kbps avg 665.412 kbps FER 0.00%
13120 frame 170560 bytes 664.951 kbps avg 665.387 kbps FER 0.00%
13144 frame 170872 bytes 665.195 kbps avg 665.343 kbps FER 0.00%
13008 frame 169104 bytes 665.109 kbps avg 665.300 kbps FER 0.00%
```

Each row prints the summary for the last period (2s) and the so-far statistics, that contains the following information:

1. Number of frames and number of bytes received in this period -- each frame has 15 bytes in the `ca` format and 13 bytes in the `usb` format.
2. Instantaneous throughput of the period and average throughput of the so-far receiving. When receiving over serial port using `au` format, the throughput should be around 960 kbps; when receiving over USB using `usb` format, the throughput should be around 665.6 kbps.
3. Frame error rate (FER) of the period: the ratio of incorrect frames over the total received frames of the period. When receiving over USB using `usb` format, you should observe FER equals zero.

