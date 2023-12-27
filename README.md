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

## Outlier detection algorithms

Find those extreme data points which are out of trend of the data sequence
itself. Voltage and current sample data come as time series, the outlier
detection alogrithm used to find those extreme voltage and current samples
contained in a large set of time series.

1. Calculate the derivative from the original series, so if data is (1, 2, 3,
   4, ...), the derivative is (0, 1, 1, ...). This is done with R function
   `diff()`. For example, if voltages stored in vector v, then the derivative
   is `dv = diff(v)`.  All the following steps will use the derivative sequence
   instead of the original sequence.

2. Define a setpoint `sp` which denotes up to how many outlier data points we want
   to find out from the `dv` set.


