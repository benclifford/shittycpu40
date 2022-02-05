shittycpu40
===========

A hobby CPU+stuff for FPGA.

For a TinyFPGA BX, which contains an iCE40.

To build
========

* Build firmware

The firmware is compiled into a .vh file which will initialise the firmware
memory with assembled code. The assembler and assemby language firmware are
in shittyasm/app/Main.hs

Use Haskell stack:

```
cd shittyasm
stack build && stack exec shittyasm-exe > ../ram.vh 
```

* Build FPGA bitstream

Uses apio to for the FPGA tool stack:

```
rm hardware.* ; apio build 
# remove hardware.* because the build system doesn't detect changes
# in the firmware file, ram.vh
```

* Install bitstream into TinyFPGA BX

```
tinyprog --program hardware.bin 
```
