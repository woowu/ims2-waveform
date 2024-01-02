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

