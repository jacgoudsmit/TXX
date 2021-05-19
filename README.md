# TXX
Fast (8mbps) serial transmitter for Propeller 1

(C) 2018 Jac Goudsmit

TERMS OF USE: MIT License.

The txx.spin module implements a fast serial transmitter for the Propeller 1, which can be used to send data to a serial port at high speed, in various formats.

The maximum theoretical speed is 8 megabits per second; however, many USB serial port adapters such as the FTDI FT-232R, can't be set to a baud rate higher than 3 megabits per second.

The module in its current form doesn't implement flow control. At extreme speeds and whenever the Propeller is connected to a computer via a hub with other USB devices, it's possible that the USB-serial chip can't keep up with the data and characters are dropped. This is not caused by this module but by hardware limitations of the USB serial chip, the USB bus and your computer.

The code was originally based on the "tx.spin" module, (C) 2011 Barry Meaker, but very little of his code is left in any recognizable form.

# How to use
To initialize the transmitter, use the Start function in Spin to set the output pin and the number of bits per second. The module uses one cog. In order to stop, you can use the Stop function to free up the cog.

The Start function returns the address of a LONG in hub memory where a command can be stored. The command contains a number of bit fields that describe what you want to send to the serial port, and in what format. The cog waits for the command to be nonzero, and then starts executing it. When it's done sending all the requested data, it clears the command long-word again. In most cases, you will want to wait for the command to clear (the Wait function can be used for this in Spin), then you store your command and you can do something else while the TXX cog takes care of business. Your code should take care not to change any data that the TXX cog is sending to the serial port while it's sending it. It doesn't copy the data before it processes it.

Because the command consists of a single LONG, it's easy to use the module from Spin as well as PASM.

# Features
The command uses bit fields to indicate what needs to be printed and how. For more details, refer to the comments in the code.
 * The data to print can consist of bytes, words, longwords or characters.
 * The print format can be unsigned decimal, signed decimal, hexadecimal or binary.
 * Characters are either sent directly, or are filtered so that unprintable characters are replaced by a period '.'.
 * The code can either print a fixed-length array of values (where your command indicates the length), or can start printing at a given location and end the printing at the first zero-value.
 * A hex dump mode is supported where a block of memory is dumped with addresses, hexadecimal values and filtered ASCII characters.

# License

MIT LICENSE

Permission is hereby granted, free of charge, to any person obtaining a
copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to permit
persons to whom the Software is furnished to do so, subject to the
following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT
OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR
THE USE OR OTHER DEALINGS IN THE SOFTWARE.
