# FPz8 - An open-source VHDL implementation of the Zilog Z8 encore

FPz8 is a softcore 100% object code compatible with the Z8 encore microcontroller line. Current implementation includes 2kb of file registers (RAM), 16kb of program memory (using FPGA RAM), 8 vectored interrupts with programmable priority, full-featured onchip debugger 100% compatible with Zilog's OCD and ZDS-II IDE.

It was designed to work as a SoC and everything (except the USB chip) fits inside a single FPGA (I have used an Altera Cyclone IV EP4CE6 device). The debugger connection makes use of a serial-to-USB chip (it is part of the low-cost FPGA board used on the project).

In a near future I plan to add some more features to the device (such as a timer and maybe other peripherals).

The idea behind the FPz8 was to learn more on VHDL and FPGAs (this is my second design using those technologies). I also believe the FPz8 can be a very interesting tool for learning/teaching about VHDL, computing and microprocessors/microcontrollers programming.

You are free to use and to modify the FPz8 to fit your needs, except for comercial use (I don't expect anyone would do that anyway). If you want to contribute to the project, contact me and share your thoughts.

Don't forget to credit the author!

Fábio Pereira

Note: currently there are only a few SFRs physically implemented, they are:

0xFC0 - IRQ0
0xFC1 - IRQ0ENH
0xFC2 - IRQ0ENL
0xFCF - IRQCTL
0xFD2 - PAIN
0xFD3 - PAOUT
0xFF8 - FCTL
0xFFC - FLAGS
0xFFD - RP
0xFFE - SPH
0xFFF - SPL

This work is licensed under the Creative Commons Attribution 4.0 International License.
To view a copy of this license, visit http://creativecommons.org/licenses/by/4.0/.
