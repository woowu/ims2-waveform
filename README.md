# Receive/Viz Picasso IMS2 A/V signal waveform

## Install the waveform receiver on PC

You need to firstly install nodejs on your Linux or Window, then change into the uncompressed directory of the ims-waveform followed by running the following command to install all the dependencies:

```
npm i
```

## Enable IMS waveform streaming on the meter

Using a meter TestApp firmware and run the below SCPI command to enable the waveform streaming:

```
IMS:WaveForm:ON
```

Power cycle the meter after issued the command.

## Receive waveforms from meter streaming serial port

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


## Receive waveforms from meter USB port

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

## Waveform data analysis
### Outlier detection algorithms

Find those extreme data points which are out of trend of the data sequence
itself. Voltage and current sample data come as time series, the outlier
detection algorithm used to find those extreme voltage and current samples
contained in a large set of time series.

1. Calculate the derivative from the original series, so if data is (1, 2, 3,
   4, ...), the derivative is (0, 1, 1, ...). This is done with R function
   `diff()`. For example, if voltages stored in vector v, then the derivative
   is `dv = diff(v)`.  All the following steps will use the derivative sequence
   instead of the original sequence.

2. Define a setpoint `sp` which denotes up to how many outlier data points we want
   to find out from the `dv` set.

