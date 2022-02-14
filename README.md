# m1lisp

I have always loved coding in assembly language because it is so close to the hardware. It teaches you so much about the hardware you are using and what it's capabilities are and what tradeoffs others have made in their programs.

I have been fascinated by Lisps for a long time as well for similar reasons. It teaches you about computation. If you think of everything as a list and you start with just a few simple rules about those lists, you can do anything. 

If you imagine that the program counter is the space in memory where the processor is loading new "list" elements, you can also think of the processor as a kind of Lisp interpreter. Marrying those two together in one project is education and a whole lot of fun.

I have read over Marc Paquette's Arpilisp several times. It's one of the best introductions to Lisps I know of. The original readme.md is included below.

The intent of this project is to update Arpilisp to run on ARM64 processors either on Linux or macOS, expand its memory, and implement numbers.


# arpilisp

Do you know how to implement a Lisp interpreter? Neither did I until I did. This is the result.

Arpilisp is a Lisp interpreter for the Raspberry Pi. It uses assembly, the lowest level language, to implement an interpreter for Lisp, the highest level language.  Arpilisp implements Lisp from scratch, with no help from any libraries and minimal help from the kernel.

It's all in a single, commented assembly file, ready to build, use, extend, and hack.

To get started you can either download [arpilisp.s](https://raw.githubusercontent.com/marcpaq/arpilisp/master/arpilisp.s) directly or clone this repository:

`git clone https://github.com/marcpaq/arpilisp`

Instructions for building and using are in the file.

Feedback is welcome and encouraged.

Enjoy.
