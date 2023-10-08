# Receive/Viz Picasso IMS2 A/V signal waveform

## Installation

npm i

## Receiving

Linux:
```
./wf-recv.mjs --device /dev/ttyUSB0 --baud 921600 --format usb --out myfile.csv

```

Windows:
```
node wf-recv.mjs --device COM5 --baud 921600 --format usb --out myfile.csv

```

The output csv file will be split into multiple files: myfile-0001.csv,
myfile-0002.csv, and so on.

## Frame formats

When receive waveform streaming from TCP. Each message
is in the form:
 ```
  0x3e | seqno | 64bit timestamp | 32 bit len | waveform chunk
```
The waveform chunk contains one or more or incompleted frames as
described below.

When receive waveform streaming from serial port. and the frame format is
'au', the stream contains frames of the form:
```
  0xa5 | A_l1 | V_l1 | A_l2 | V_L2 | A_l3 | V_l3 | chksum 
```
All A/V quantities are 16-bit integer in little endian. Checksum is 16-bit.

When receive waveform streaming from serial port. and the frame format is
'usb', the stream contains frames of the form:
```
  0xa5 | A_l1 | V_l1 | A_l2 | V_L2 | A_l3 | V_l3
```
All A/V quantities are 16-bit integer in little endian. Checksum is 16-bit.

