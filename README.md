



# ETI-Simple

By Dr Tim Hendtlass   RMIT Melbourne Australia

A Tiny Language for 8080/Z80 derived from PILOT, published in Electronics Today International, January 1979

Converted to Z80 mnemonics by Ken Boak September 2021

#### Instructions:

Download the appropriate hex file to your Southern Cross or TEC-1F using the serial monitor in the Southern Cross Monitor.

Run the program from $2000, the program will do a CR LF and it's ready to go!

The program is stored in memory as strings with a CR terminator and has an 'entry pointer' that allows lines to be entered and edited in 'Command Mode'.

There are five commands;

**Backspace**  = deletes the previous character.

**&** = Display the current line.

**%** = Pad routine  fills the line to the CR with nulls.

**$** = Executes the code from the current entry pointer location.

**#** = Reset the  entry pointer to the beginning of the program.

You can download the example file nim.spl as a plain text file, add a transmit delay to the character and the line ( I used 25ms/char and 250ms/line as a guess) Don't forget to remove these delays when you want to download another executable file!

Once the program has downloaded press # to return the entry pointer to the top of the program, then press $ to run it.

