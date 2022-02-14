; For Marc Paquette's amazing documentation and introductions to LISP and ARM32 assembly, see the original 
; source, included in this repository.
;
;
; Original Source Copyright (c) 2016 Marc Paquette, MIT licensed
; Subsequent commits from 6907611fadb3e6dd2bd405a664d3f8275912e6f5 onward are
; (c) 2022 Jon-Erik G. Storm, LGPL2 licensed

.arch ARMv8.4
.p2align 2	; 	Everything needs to be aligned on 32-bits/4 bytes


	/*
	________________________________________________________________________________

	Procedures in Assembly

	These assembler macros tell the assembler, the debugger, and us where a
	procedure starts and ends.  They also make sure that the assembler aligns
	instructions properly and places constant values that our procedure refers to in
	a place that the procedure can reach.

	Reference

	https://community.arm.com/docs/DOC-9652

	*/

	.macro PROC name
	.text
	.balign 4
	.func \name, \name
\name\():
	.endm

	.macro ENDPROC
	.pool
	.endfunc
	.endm

	/*
	________________________________________________________________________________

	Errors

;; The original code uses flags to store state, but also uses some registers.
;; Just to keep it clean, I'm going to switch to registers.

	We need a way to flag an error. We'll use the overflow (V) flag in the apsr.
	This gives us a simple mechanism to set and detect error conditions.  Reacting
	to an error is as simple as appending the "vs" or "vc" condition code to
	affected instructions.

	Our error-flagging method is blunt--it clears all condition flags along with
	setting or clearing V.  As long as we keep that in mind, we won't get stung.

	*/

	.macro ERRSET errsym
;; --	msr apsr_nzcvq, #1<<28  --
	mov x28, #1<<28
	ldr x7, =\errsym
	.endm

	.macro ERRCLR
;; --	msr apsr_nzcvq, #0 --
	xor x28, x28, #1<<28	
	.endm

	/*
	________________________________________________________________________________

	Returning boolean results

	We use the Z flag in the status register to handle true and false results.  When
	we set the Z flag (and bluntly clear other conditions), we can test for it with
	the EQ condition flag.  Otherwise, we clear the status registers to use the NE
	condition.

	*/
	.macro ZSET
;; --	msr apsr_nzcvq, #1<<30 --;;
	mov x28, #1<<30
	.endm
	
	.macro ZCLR
;; --	msr apsr_nzcvq, #0 --;;
	xor x28, x28, #1<<30
	.endm

	/*
	________________________________________________________________________________

	Lisp stack

	We need a safe place to store intermediate Lisp values.  We use a stack for
	this.  We cover the idea of "safe" later on.

	We reserve some memory for the Lisp stack and x6 for the Lisp stack pointer.
	This stack pointer is separate from processor's stack pointer, aka sp, aka x13.

	Side note: The Linux kernel conveniently sets up sp for us and reserves some
	memory for the process stack when it launches arpilisp.

	*/
	.equiv LISPSTACKMAX, 1000	/* Number of pointers to Lisp objects. */

	.bss
	.balign 4
lispstackbottom:
	.space LISPSTACKMAX * 4
	.equiv lispstacktop, .

	/*

	Push the Lisp value in x0 onto the Lisp stack.  Signal an error if the stack is
	full.  Generating an error instead of panicking is fine because it lets our
	interpreter gracefully recover from exhausted Lisp stack space.

	*/
	
	PROC pushlisp
;; --	push { x1 }	-- ;;
;; We have to replace the push instructions with either manual saving or a macro.
	ldr x1, =lispstackbottom
	cmp x6, x1
	bne 10f
	ERRSET errstackfull
	b 999f

10:	stmfd x6!, { x0 }
	ERRCLR

999:	pop { x1 }
	mov pc, lr
	ENDPROC

	/*

	Pop from the Lisp stack and store it in x0.  Modifies the condition flags.

	Panic if we try to pop from an empty stack.  We panic instead of generating an
	error because a pop requires a previous push.  In other words, a mismatched pop
	implies a bug in the interpreter's logic, not the Lisp program that it is
	interpreting.

	*/

	PROC poplisp

	ldr x0, =lispstacktop
	cmp x6, x0
	bne 10f
	ldr x0, =panicstackempty
	b panic

10:	ldmfd x6!, { x0 }
	mov pc, lr
	ENDPROC

	
	For simplicity of implementation, a practical value for nil is 0.  We store
	nothing useful at memory location 0.

	Side note: In fact, a Linux user program cannot store or load location 0.  The
	ARM Linux kernel doesn't allow it by design.  Doing so results in a segmentation
	fault, which is an intentional thing.

	Reference

	https://www.kernel.org/doc/Documentation/arm/memory.txt

	*/
	.equiv NIL, 0

	/*

	We keep all our cells in a pool.  A cell occupies 2 words = 2 * 4 bytes = 8
	bytes.

	*/
	.bss
	.balign 4

	.equiv CELLMAX, 10000

cells:
	.space CELLMAX * 2 * 4
	.equiv cellsend, .

	/*

	There are two ingredients to marking and sweeping: a free list and a root set.

	First ingredient: the free list.  The free list points to abandoned cells that
	are ready to use.  When a Lisp program needs a new cons cell, Lisp removes it
	from the free list and gives it to the program.

	*/

freelist:
	.word NIL

	/*

	When we start the interpreter, we have to build the free list.  We just iterate
	through the cell pool, pointing each cons cell's cdr to the next cons cell.  At
	the last cons cell, we point to nil.

	When it's initialized, our free list initially looks like this:
	
	             +-----+-----+   +-----+-----+            +-----------+
	freelist --->| car | cdr --->| car | cdr ---> ... --->| car | cdr --->nil 
	             +-----+-----+   +-----+-----+            +-----+-----+   

	*/

	PROC initfreelist

	push { x0-x2, lr }
	ldr x0, =cells
	ldr x1, =freelist		/* Free list starts at the beginning */
	str x0, [ x1 ]			/* of the cell pool. */

	ldr x2, =cellsend
	sub x2, x2, #8			/* Stop before the last cell. */

1:	mov x1, x0
	add x0, x0, #8			/* The next cell. */
	str x0, [ x1, #4 ]
	cmp x0, x2
	bne 1b

	pop { x0-x2, pc }

	ENDPROC

	/*

	Second ingredient in garbage collection: the root set.  The root set is the
	starting point for marking.  Lisp follows the cells pointed to by the cells in
	the root set, travelling through all referred cells.

	In other words, all cells that are reachable from the root set, directly or
	indirectly, are safe from garbage collection.  Cells that are not reachable are
	added to the freelist.

	The garbage collector starts by marking all used cells.  This is the mark in
	mark-and-sweep.

	During the mark phase, Lisp could encounter a cell that it has already
	marked.  In this case, Lisp stops chasing further because we know that
	subsequent, reachable cells from a marked cell are already marked.

	To mark a cons cell, we must make sure not to corrupt the original data in the
	cell.  We do this by taking advantage of a quirk in ARM's data alignment
	preferences.  Genuine ARM pointers are aligned to the nearest word (4 bytes), so
	a pointer's 2 least significant bits are always 0.

	ARM pointer
	+--------+--------+--   --+-------+-------+
	| 0 or 1 | 0 or 1 |  ...  |   0   |   0   |
	+--------+--------+--   --+-------+-------+
	  bit 31   bit 30           bit 1   bit 0 
	
	We take advantage of these 2 bits to encode information about a Lisp object in
	the actual pointer to the object.

	To specify that a cell is in use during the mark phase, we mark its car by
	settings bit 1 to 1.  We have another use for bit 0, covered later.

	We don't need to worry about decoding a marked cell to refer to its pointer for
	a few reasons: our Lisp program is suspended during mark and sweep, we don't follow
	reachable cells from a marked cell, and we only use the mark bit during the
	marking phase of garbage collection.

	*/
	.equiv MARKMASK, 0b10

	PROC collectgarbage

	push { x0, x6, lr }

	/* Mark the root set. */
	mov x0, x9			/* The environment. */
	bl mark
	mov x0, x8			/* The expression being evaluated. */
	bl mark
	mov x0, x7			/* The value. */
	bl mark
	mov x0, x5			/* The list of lambda argument values. */
	bl mark
	ldr x0, =freelist
	ldr x0, [x0]
	bl mark

	/* Mark the Lisp stack. */
10:	ldr x0, =lispstacktop	
	cmp x6, x0
	beq 20f
	ldmfd x6!, { x0 }
	bl mark
	b 10b

20:	bl sweep
	pop { x0, x6, pc }

	ENDPROC

	/*

	Given a pointer in x0, mark it if applicable.

	*/

	PROC mark

	push { x0-x3, lr }

	/* Does x0 point to a cell? */
1:	cmp x0, #NIL
	beq 999f
	tst x0, #SYMMASK
	bne 999f

	/* Is the car already marked? */
	ldr x1, [ x0 ]
	tst x1, #MARKMASK
	bne 999f

	/* Mark the car. */
	mov x2, x1			/* Mark with x2, save x1. */
	orr x2, x2, #MARKMASK
	str x2, [x0]			/* Store the mark in car. */

	/* Follow the data pointed to by the car and cdr. */
	mov x3, x0			/* Save original pointer in x3. */
	mov x0, x1			/* Chase after the car. */
	bl mark
	ldr x0, [ x3, #4 ]		/* Chase after the cdr. */
	b 1b

999:	pop { x0-x3, pc }
	ENDPROC

	/*

	With marking completed, the interpreter starts sweeping.  It iterates through
	the cell pool, unmarking active cells and inserting inactive cells into the free
	list.

	*/
	PROC sweep

	push { x0-x1, lr }
	ldr x1, =cells
1:	ldr x0, [ x1 ]			/* Is the car marked? */
	tst x0, #MARKMASK
	beq 2f

	/* Cell is active, unmark it. */
	bic x0, #MARKMASK
	str x0, [ x1 ]
	b 3f

	/* Cell is inactive, add it to freelist. */
2:	mov x0, #0			/* Clear the car. */
	str x0, [ x1 ]
	ldr x0, =freelist		/* Store freelist head in the cdr. */
	ldr x0, [ x0 ]
	str x0, [ x1, #4 ]
	ldr x0, =freelist		/* Point freelist to the new head. */
	str x1, [ x0 ]

	/* Next cell in the cell pool. */
3:	ldr x0, =cellsend
	add x1, x1, #8
	cmp x1, x0
	bne 1b

	pop { x0-x1, pc }

	ENDPROC

	/*

	Lisp provides the cons function to allocate new cells.  A Lisp program uses cons
	to construct its data.  This is where the free list comes in.
	
	             +-----+-----+   +-----+-----+            +-----------+
	freelist --->| car | cdr --->| car | cdr ---> ... --->| car | cdr --->nil 
	             +-----+-----+   +-----+-----+            +-----+-----+   
		         cell 1          cell 2                   cell N


	Allocating a cons cell from the freelist is simple.  We just "pop" it from the
	freelist:
	
                     allocated cell
	                   |
	                   V
	             +-----+-----+     +-----+-----+            +-----------+
	freelist -+  | car | cdr |  +->| car | cdr ---> ... --->| car | cdr --->nil 
	          |  +-----+-----+  |  +-----+-----+            +-----+-----+   
		  |      cell 1     |     cell 2                   cell N
		  |		    |	
	          +-----------------+
	
	Given pointers to Lisp objects in x1 and x2, return an allocated cell in x0 such
	that its car contains x1 and cdr contains x2.  Otherwise, panic if there is no
	memory or issue an error if the Lisp stack is full.

	*/

	PROC cons

	push { x3-x4, lr }
	ldr x3, =freelist		/* Is the free list empty? */
	ldr x0, [ x3 ]
	cmp x0, #NIL
	bne 10f

	
	mov x0, x1
	bl pushlisp
	bvs 999f
	mov x0, x2
	bl pushlisp
	bvs 999f
	bl collectgarbage
	bl poplisp
	mov x2, x0
	bl poplisp
	mov x1, x0

	ldr x0, [ x3 ]			/* Is the free list still empty? */
	cmp x0, #NIL
	bne 10f
	
	ldr x0, =panicmemfull
	b panic

10:	ldr x4, [ x0, #4 ]		/* Advance freelist. */
	str x4, [ x3 ]
	str x1, [ x0 ]			/* Store the car and cdr. */
	str x2, [ x0, #4 ]
	ERRCLR
999:	pop { x3-x4, pc }

	ENDPROC

	/*
	________________________________________________________________________________

	Symbols

	In arpilisp, our simplest object, after the empty list, is the symbol.  A symbol
	is just a reference to the character string of itself.

	Side note: Most other Lisps allow the programmer to attach properties to a
	symbol.  For example, a symbol has a property called the print name, which is
	the textual representation of the symbol.  To print a symbol, the Lisp
	interpreter retrieves the print name property and outputs it.  The property
	mechanism is overkill for arpilisp.  Instead, we use a symbol as a reference to
	its print name.

	The format we use for print names is the length of its print name encoded as a
	word-aligned word, followed by the character string of the print name itself.
	The next print name follows after that, aligned to a word boundary, and so on.

	symbol 1			 	 symbol 2
	+-----------+------------+--------------+---- - - -
	| length    | characters | padding      | length
	+- 4 bytes -+- n bytes --+- < 4 bytes  -+---- - - -

	We store symbol print names in a block of memory.  The traditional name for this
	block is "obarray."

	*/

	.equiv OBARRAYMAX, 2000	/* Multiple of 4, < 2^30. */

	.data
	.balign 4
obarray:

	/* Keep track of the last unused position in obarray. */
	.balign 4
	.set OBARRAYEND, .

	/*

	We have 2 types of object: symbols and cells.  We need a way to determine if a
	pointer refers to an object that is appropriate for the current computation.  For
	example, if a Lisp function expects a symbol as an argument, we want to ensure
	that the argument is indeed a symbol and not a cell or nil.

	We mentioned earlier that the least 2 significant bits in an ARM pointer are 0.
	We already use bit 1 as the mark bit for garbage collection.  That leaves us
	with the least significant bit, bit 0, to specify the type of object that a
	pointer refers to.  When bit 0 is 1, the pointer refers to a symbol, not a cell
	and not nil either.

	*/

	.equiv SYMMASK, 1

	/*

	A reference to a symbol is not really a pointer.  To decode a reference to a
	symbol into a usable pointer, we mask out the flag and add the address of
	obarray.
	
	Why not use a real pointer instead of addding the address of obarray?  It turns
	out that setting up the initial symbols we need for arpilisp poses a challenge.
	Ideally, we would like to pre-fill obarray at assembly time instead of runtime.
	But the assembler won't let us perform assembler-time arithmetic on labels
	because the ELF binary format and the Linux kernel conspire to force the
	calculations of final addresses only at runtime.

	We could hard-code the addresses of Lisp symbols at assembly time, but we'd have
	to either make arpilisp more complicated or depend on some uncomfortable
	assumptions about the memory map of a Linux process.

	However, the assembler happily lets us compute offsets from assembly labels.
	Our solution, then, is to treat a Lisp symbol pointer not as a pointer but as an
	offset into obarray.

	The LISPSYM macro appends a symbol entry to obarray.  We use it to pre-define
	our initial set of symbols.  We may only apply this macro immediately
	after the definition of obarray.  So pre-define all your symbols here.

	*/
	.macro LISPSYM sym, printname
	.data
	.balign 4
	.equiv \sym , (. - obarray) | SYMMASK
	.word \sym\()_STREND - \sym\()_STRSTART
	.balign 4
	.equiv \sym\()_STRSTART, .
	.ascii "\printname"
	.equiv \sym\()_STREND, .
	.balign 4
	.set OBARRAYEND, .
	.endm

	LISPSYM symnil, "nil"
	LISPSYM symt, "t"
	LISPSYM symquote, "quote"
	LISPSYM symatom, "atom"
	LISPSYM symeq, "eq"
	LISPSYM symcar, "car"
	LISPSYM symcdr, "cdr"
	LISPSYM symcons, "cons"
	LISPSYM symcond, "cond"
	LISPSYM symdefine, "define"
	LISPSYM symlambda, "lambda"
	
	LISPSYM errdot, "Error: unexpected dot "
	LISPSYM errparenmissing, "Error: expected closing parenthesis "
	LISPSYM errparen, "Error: unexpected closing parenthesis "
	LISPSYM errbadlistexpr, "Error: unknown expression "
	LISPSYM errcellornil, "Error: expected a cell or nil "
	LISPSYM errbindsym, "Error: expected a symbol to bind "
	LISPSYM errbadsym, "Error: expected a valid symbol "
	LISPSYM errunboundvar, "Error: unbound variable "
	LISPSYM errargextra, "Error: too many arguments "
	LISPSYM errargmissing, "Error: missing arguments "
	LISPSYM errstackfull, "Error: stack full "
	
	LISPSYM panicstackempty, "Panic: stack empty"
	LISPSYM panicobarrayfull, "Panic: obarray full"
	LISPSYM panicmemfull, "Panic: memory full"

	/* Fill the rest of the obarray space. */
	.space OBARRAYMAX - (. - obarray)
	.set OBARRAYCAP, .

	/* Remember the last unused position in obarray. */
obarrayend:
	.word OBARRAYEND

	/*

	A useful operation for symbols is to compare them for equality.  Given two
	symbols, are they the same?

	One way to do this is to compare their print names, character by character.

	A much simpler and faster way is to compare the symbols' pseudo-pointers.

	Remember the eq function earlier?  Implementing this function in assembler is
	dead easy: just compare two pointers.  For cells and nil, this work well.  We
	would like to have this same convenience and efficiency for symbol comparison.
	But there's a down side: for this simple symbol comparison, we need to make sure
	that symbols are unique, which means making sure that we only record a single
	copy of a symbol.

	Side note: The up side of storing a single copy of a print name is that we save
	memory, which is, incidentally, why we don't bother with garbage collection of
	symbols.

	In Lisp, we say that this storage of only one instance of a symbol is
	"interning" a symbol.  To create a new symbol, either return a reference to the
	single instance of a symbol in obarray or, if the symbol has not yet been
	interned, create the symbol in obarray and return the reference.

	The buffer for a symbol to intern is internbuffer.  We use it as a temporary
	place to store a symbol from input.  We structure it like a symbol in obarray,
	which means a word-aligned word-sized length value immediately followed by a
	character string.

	*/
	.equiv INTERNMAX, 32		/* A multiple of 4 */

	.bss
	.balign 4
internbufferlen:
	.word 0
	.balign 4
internbuffer:
	.space INTERNMAX

	/*

	Return the pseudo-pointer to the symbol in obarray that matches the string in
	internbuffer.  If the symbol does not exist, create it in obarray and return the
	symbol's pseudo-pointer.

	*/

	PROC intern

	push { x0-x5, lr }

	/* Search for the symbol in obarray. */
	ldr x0, =obarray		/* Start of obarray. */

1:	ldr x1, =obarrayend		/* At the end of obarray? */
	ldr x1, [ x1 ]
	cmp x0, x1
	beq 5f

	mov x1, x0
	ldr x2, =internbufferlen

	/* Compare print name lengths. */
	ldr x3, [ x1 ], #4		/* Length of print name in obarray. */
	ldr x4, [ x2 ], #4		/* Length of print name to intern. */
	cmp x3, x4
	bne 4f

	/* Compare print names. */
2:	cmp x3, #0			/* Have we compared all characters? */
	beq 8f				/* Found.  Symbol already interned. */

3:	ldrb x4, [ x1 ], #1
	ldrb x5, [ x2 ], #1
	cmp x4, x5
	bne 4f
	sub x3, x3, #1
	b 2b

	/* Advance to the next print name in obarray. */
4:	ldr x1, [ x0 ]			/* Length of the current string. */
	add x1, x1, #4			/* Advance to the first character. */
	add x1, x1, x0			/* Add string's address. */

	/*

	At this point, x1 is at the end of the symbol object that x0 points to.  The next
	symbol object is at the next 4-byte boundary.  We need to round up to this
	boundary by calculating how much to add to x1.  The bad news is that ARM doesn't
	have an integer modulo instruction.  The good news is that 4 is a power of 2, so
	bit manipulation is the obvious choice and ARM has plenty of useful
	bit-manipulation instructions.

	To calculate an offset to add to x1, we want this mapping of the least 2 bits:

	0b00 -> 0b00
	0b01 -> 0b11
	0b10 -> 0b10
	0b11 -> 0b01

	Note that for 0b00, we're already there so there's nothing to add to x1.

	*/

	and x0, x1, #0b11		/* Consider only the least 2 bits. */
	rsb x0, x0, #4			/* Subtract 4. */
	and x0, x0, #0b11		/* Keep only the least 2 bits. */

	add x0, x0, x1			/* Point x0 to next object. */

	b 1b

	/*

	Allocate a new print name in obarray.

	At this point, both x0 and x1 contain the value stored at obarrayend.  This
	value points to our new symbol, which we return in x0.  So we preserve x0 for
	that purpose.

	*/

5:	ldr x3, =internbufferlen

	/* Is there room in obarray? */
	add x1, x1, #4			/* Add the word for the length. */
	ldr x2, [ x3 ]
	add x1, x1, x2			/* Add the length of the string. */
	and x2, x1, #0b11		/* Align to the next 4-byte boundary. */
	rsb x2, x2, #4
	add x1, x1, x2
	ldr x2, =OBARRAYCAP
	cmp x1, x2
	blt 6f

	/* No more symbol room. */
	ldr x0, =panicobarrayfull
	b panic

	/* Expand obarray. */
6:	ldr x4, =obarrayend
	str x1, [ x4 ]

	mov x1, x0			/* Reset x1 to our new symbol. */

	ldr x2, [ x3 ], #4		/* Copy the length. */
	str x2, [ x1 ], #4

	/* Copy the string. */
7:	cmp x2, #0
	beq 8f
	ldrb x4, [ x3 ], #1
	strb x4, [ x1 ], #1
	sub x2, x2, #1
	b 7b

	/* Encode our pointer as a symbol. */
8:	ldr x1, =obarray		/* Subtract obarray base. */
	sub x7, x0, x1
	orr x7, x7, #SYMMASK		/* Imprint the mask. */

	pop { x0-x5, pc }

	ENDPROC

	/*

	________________________________________________________________________________

	Output

	We need to print symbols, cells, and nil.  Here is where we get to see the
	internal representations of cons cells and symbols meet their external Lisp
	counterparts.  We've seen hints earlier, but now we get to clearly see how lists
	are made of cons cells.

	To print a pointer to a Lisp object, let's take a first stab at syntax rules:

	pointer = 'nil' | symbol | cell 

	cell = '(' car '.' cdr ')'

	car = pointer

	cdr = pointer

	A few things about the first draft of our syntax rules:

	* A cell's textual representation starts and ends with parentheses.

	* We separate the car and cdr with a period.  In Lisp, we call this "dot
	notation".

	* A list is a recursive definition; a car and cdr may each point to other cells
	via the syntax rule, pointer. 

	Example output from these rules:

	* Printing nil produces the output of "nil".

	* Printing a symbol produces the symbol's character string.

	* Let A be a cons cell with car pointing to symbol "abacus" and cdr pointing to
	symbol "anchovy".  Output: "(abacus . anchovy)".

	A
	+-----+-----+
	| car | cdr ----> anchovy
	+--|--+-----+
	   |
	   v
	abacus

	* Let B be a cons cell in which the car points to symbol "beluga" and cdr points
	to nil.  Printing B gives "(beluga . nil)".

	B
	+-----+-----+
	| car | cdr -----> nil
	+--|--+-----+
	   |
	   v
	beluga

	* Let C be a cons cell with car pointing to symbol "cantilever" and cdr pointing
	to cell B.  Its output: "(cantilever . (beluga . nil))".

	C                B
	+-----+-----+  	 +-----+-----+
	| car | cdr ---->| car | cdr ----> nil
	+--|--+-----+	 +--|--+-----+
	   |		    |
	   v		    v
	cantilever	 beluga

	These format rules are perfectly useful and accurate.  But notice that they
	don't quite give the output for lists that we saw earlier.  Our rules produce a
	lot of dots and parentheses for cons cells structures that barely approach
	complexity.

	Now you know why Lisp is sometimes referred to as "Lots of Irritating, Stupid
	Parentheses."

	Let's add a couple of rules to make the output a little cleaner. 

	Our new rules:

	pointer = 'nil' | symbol | cell 

	cell = '(' cell-contents ')'

	cell-contents = car (cdr-nil | cdr-cell | cdr-symbol)

	car = pointer

	cdr-nil = empty-string

	cdr-cell = ' ' cell-contents

	cdr-symbol = ' . ' symbol

	Note our changes:
	
	* When the cdr is nil, don't print the dot and don't print the 'nil'.

	* When the cdr points to a cell, we don't print a dot or the left
	parenthesis of the cons cell that the cdr points to.

	* Only when the cdr points to a symbol, do we print a dot, followed by the
	symbol.
	
	With these updated rules our example output becomes:

	* Nil is still output as "nil".

	* A symbol is still output as itself.

	* A: "(abacus . anchovy)"

	* B: "(beluga)"

	* C: "(cantilever beluga)"

	Now our output rules are consistent with what we expect about list output.

	Before we start printing, let's define some character strings to match our
	rules.

	*/

	.balign 4
leftparenstr:
	.ascii "("
	.equiv LEFTPARENSTRLEN, . - leftparenstr

	.balign 4
rightparenstr:
	.ascii ")"
	.equiv RIGHTPARENSTRLEN, . - rightparenstr

	.balign 4
spacestr:
	.ascii " "
	.equiv SPACESTRLEN, . - spacestr

	.balign 4
dotstr:
	.ascii " . "
	.equiv DOTSTRLEN, . - dotstr

	/*

	Most Lisp implementations provide prin1 and print functions.  The prin1
	procedure outputs a Lisp object without a newline ending.

	Given a pointer to a Lisp object in x0, output its textual representation.

	*/

	PROC prin1

	push { x0-x3, lr }

	cmp x0, #NIL			/* Is this nil? */
	bne 1f

	/* Print nil. */
	ldr x0, =symnil
	b 2f

1:	tst x0, #SYMMASK		/* Is this a symbol? */
	beq 3f

	/* Print a symbol. */
2:	bic x0, #SYMMASK
	ldr x1, =obarray
	add x0, x0, x1
	add x1, x0, #4			/* Symbol string address. */
	ldr x2, [ x0 ]			/* Symbol length. */
	bl write
	b 999f				/* Done. */

	/* Print a cell. */
3:	ldr x1, =leftparenstr
	ldr x2, =LEFTPARENSTRLEN
	bl write

4:	mov x3, x0			/* Save our cell pointer. */
	ldr x0, [x3]			/* Get the car and print it. */
	bl prin1
	ldr x0, [x3, #4]		/* Get the cdr. */
	cmp x0, #NIL			/* Is the cdr nil? */
	beq 6f
	tst x0, #SYMMASK	/* Is the cdr a symbol? */
	bne 5f
	ldr x1, =spacestr		/* Cdr is a cell, so iterate. */
	ldr x2, =SPACESTRLEN
	bl write
	b 4b

5:	ldr x1, =dotstr			/* Cdr is a symbol, so print the dot. */
	ldr x2, =DOTSTRLEN
	bl write
	bl prin1			/* Print the symbol. */

6:	ldr x1, =rightparenstr
	ldr x2, =RIGHTPARENSTRLEN
	bl write

999:	pop { x0-x3, pc }

	ENDPROC

	/*

	Print an end-of-line.  In Lisp, outputing an end-of-line is traditionally
	referred to as "terminate the print line" or "terpri."

	*/

	.data
	.balign 4
eolstr:
	.ascii "\n"
	.equiv EOLSTRLEN, . - eolstr

	PROC terpri

	push { x1, x2, lr }
	ldr x1, =eolstr
	ldr x2, =EOLSTRLEN
	bl write
	pop { x1, x2, pc }

	ENDPROC

	/*

	Print an object with newlines around it.

	*/

	PROC print

	push { lr }
	bl prin1
	bl terpri
	pop { pc }

	ENDPROC

	/*
	________________________________________________________________________________

	Low-Level I/O
	
	With the logic to format S-expressions for output, we need a way to actually get
	this information into the real world for the user to see.  This is the part
	where we implement I/O.

	Notice that the gcc command we use earlier includes the -nostdlib option.  This
	tells gcc not to link to the standard system and startup libraries.  These
	libraries pile layers of convenient wrappers and lots of infrastructure between
	a user program and the kernel's low-level services.

	We choose to avoid that.  Instead, we call the linux kernel directly through its
	system call interface.  Each system call has a unique number.  Most system calls
	also need parameters.  To cross the border between a user program and the
	kernel, we load the system call's number and parameters into registers then use
	the svc instruction to take the leap into the kernel.  The kernel does its magic
	and returns execution to the user program at the next instruction after svc.

	Arpilisp uses a minimum of system calls.  The full list is here:

	/usr/src/arch/arm/kernel/calls.S

	For output, we use sys_write, specified by x7, and the standard output file, in
	x0.  The kernel also requires that we specify the address of the buffer to write
	from in x1 and the length of the buffer in x2.

	*/

	PROC write

	push { x0, x7, lr }
	mov x7, #4			/* sys_write */
	mov x0, #1			/* stdout */
	svc #0
	pop { x0, x7, pc }

	ENDPROC

	/*
	________________________________________________________________________________

	Input

	In Lisp, the read function converts input characters into an S-expression.

	We read a little differently than we print.  Printing is simple: we already
	know everything we need to print.  But when we read, we don't know what it is
	until it's read completely.

	Most books on programming language implementation divide the problem of input
	processing into lexical analysis and parsing.  A lexical analyzer groups
	characters into tokens.  A parser then groups tokens into expressions,
	statements, and the other higher-level constructs of a high-level language.

	S-expression syntax, especially for arpilisp, is simple enough not to worry
	about a formal separation of lexical analysis and parsing.  Lexical analysis for
	arpilisp has only these things to tokenize:

	* opening parenthesis

	* closing parenthesis

	* dot

	* symbol

	The parentheses and dot characters are easy to read: when you read one of them,
	you're done reading the token.

	A symbol token is composed of one or more sequential characters. This is a
	problem because we can read only a single character at a time.  When we get the
	first character that forms a symbol, we have no way to know if we have read all
	of the symbol.  The only way to confirm whether we have read a complete symbol
	or not is to read the next character.

	For example, we read an "h" character.  This "h" might be a complete symbol.
	But it might not be, so we are compelled to keep reading.  We read the next few
	characters, which are "a" then "t" then " " (space).  The space is not part of
	the symbol, which tells us conclusively that the previous three characters form
	the symbol "hat".

	Reading the space means we have gone too far. We have read a character that we
	can't use now but will need later for the next token.  We need some way to
	"unread" this character.

	In Unix-influenced systems, there is exactly such a standard library function,
	ungetc().  This function puts a character back in a file so that it can be
	re-read later.

	It's a funny way to go about things, because the character really isn't put back
	in the file.  The kernel offers no such capability for practical and technical
	reasons.  It's actually the standard library that fakes an "unget".  Implementing
	ungetc() requires some trickery in a higher-level language, and especially
	in assembly language.

	Instead, we use a simpler alternative, excellently described by Jack W. Crenshaw
	in his Let's Build a Compiler articles.  The Crenshaw method uses a look-ahead
	variable, Look, and a function, GetChar.  Look contains the character that the
	lexical analyzer is currently considering.  GetChar updates Look by reading the
	next character in a file.  In other words, only when we are sure that we can use
	the Look character do we bother to read the next character, where it is safely
	held in Look to be analyzed when it's needed.

	Reference

        http://www.compilers.iecc.com/crenshaw/

	*/

	.data
	.balign 4
lookbuffer:
	.ascii "\n"			/* Force a getchar and a prompt. */

	.equiv EOT, 4			/* Ctrl+D, end of transmission. */

	.macro LOOK reg
	ldr \reg, =lookbuffer
	ldr \reg, [ \reg ]
	.endm

	/*

	Before we get to actually reading S-expressions, we need some lower-level
	procedures.

	To read characters from standard input, we use the sys_read system call.

	*/

	PROC getchar

	push { x0-x2, x7, lr }
	ldr x1, =lookbuffer
	mov x2, #1
	mov x7, #3			/* sys_read */
	mov x0, #0			/* stdin */
	svc #0
	cmp x0, #0			/* End of file? */
	moveq x0, #EOT
	streq x0, [ x1 ]
	pop { x0-x2, x7, pc }

	ENDPROC

	/*

	We need procedures to validate characters that we read.  For example, we need to
	know if a character can be used for a symbol.

 	In arpilisp, a valid symbol character is any graphical character that is not
	otherwise part of Lisp syntax.  This means any string of characters is allowed
	in a symbol excluding control characters (which includes white space), the
	opening and closing parentheses, and dot.
 
	This function only works within the Unicode Basic Latin block, aka
	ASCII.  We could make the effort to support Unicode more substantially, but we
	want to focus on Lisp in assembly, not Unicode.
	
	*/

	PROC issym
	push { lr }
	cmp x0, #'('
	beq 20f
	cmp x0, #')'
	beq 20f
	cmp x0, #'.'
	beq 20f
	cmp x0, #EOT
	beq 20f
	cmp x0, #'!'
	blt 20f
	cmp x0, #'~'
	bge 20f
	ZSET				/* A valid symbol character. */
	b 999f
	
20:	ZCLR				/* Not valid. */

999:	pop { pc }
	ENDPROC


	/*

	Is the character in x0 white space?  Notice that we don't use the ZSET and
	ZCLR macros because the cmp instruction sets Z for us.

	*/
	PROC iswhite
	push { lr }
	cmp x0, #' '
	beq 999f
	cmp x0, #'\t'
	beq 999f
	cmp x0, #'\n'
999:	pop { pc }
	ENDPROC

	/*

	We enhance our lexical analyzer to recognize white space for our own human
	convenience.

	*/

	PROC skipwhite

	push { x0, lr }
1:	LOOK x0
	bl iswhite
	bne 999f
	bl getchar
	b 1b
99:	pop { x0, pc }

	ENDPROC

	/*

	Now we're ready to read S-expressions. We use the same syntax rules that we
	use for output.

	Note how we save and restore x6, the Lisp stack pointer.  We do this to simplify
	error handling; instead of being careful about unwinding the Lisp stack to handle
	an error, we just reset x6 to its starting point.

	Also notice how read is a wrapper. It does nothing but save x6 then call read1,
	which does the heavy lifting.  The reason is because of lists: a list may
	contain symbols, nil, and other lists.  To implement this definition, we need a
	procedure that can call itself, read1.

	Read an S-expression, returning a pointer to it in x7.

	*/

	PROC read

	push { x6, lr }
	bl read1
	pop { x6, pc }

	ENDPROC

	/*

	Read an S-expression.

	*/
	PROC read1

	push { x0, lr }
	bl skipwhite
	LOOK x0

	/* End of file.  Return to the OS. */
	cmp x0, #EOT
	beq finish

	/* We aren't ready for a dot here. */
	cmp x0, #'.'
	bne 1f
	bl getchar			/* Eat the dot. */
	ERRSET errdot
	b 999f

	/* We aren't ready to close parenthesis either. */
1:	cmp x0, #')'
	bne 2f
	bl getchar			/* Eat the closing parenthesis. */
	ERRSET errparen
	b 999f

2:	cmp x0, #'('			/* Read a cell? */
	bne 3f
	bl readcell
	bvs 999f
	b 990f

3:	bl issym
	beq 4f
	bl getchar			/* Eat the non-symbol character. */
	ERRSET errbadsym
	b 999f
4:	bl readsym
	bvs 999f
	
990:	ERRCLR
999:	pop { x0, pc }

	ENDPROC

	/*

	Read a cell, returning a pointer to it in x7.

	Remember that our print rule for cell-contents may contain a car, which is a
	pointer, which may refer to a cell.  This mutual recursion means we allow an
	arbitrarily deep number of cars before we get to the cdr-nil, cdr-symbol, or
	cdr-cell rules.

	*/

	PROC readcell

	push { x0-x3, lr }
	bl getchar			/* Eat the opening parenthesis. */
	bl skipwhite

	LOOK x0				/* Immediate closing parenthesis? */
	cmp x0, #')'
	bne 5f
	mov x7, #NIL
	b 990f

	/* Read the first object in our list. */
5:	bl read1
	bvs 999f

	/* Construct a list comprising only of a pointer to our first object. */
	mov x1, x7
	mov x2, #NIL
	bl cons
	mov x7, x0
 	add x3, x0, #4			/* Point to the cdr of our list. */

10:	bl skipwhite			/* Are we at the end of our list? */
	LOOK x0
	cmp x0, #')'
	beq 990f

 	cmp x0, #'.'			/* Are we at a dot? */
 	beq 30f

20:	mov x0, x7			/* Read the next object in our list. */
	bl pushlisp
	bvs 999f
	bl read1
	bvs 999f

	mov x1, x7			/* Append the object's pointer to our list. */
	mov x2, #NIL
	bl cons
	str x0, [ x3 ]			/* Point the cdr to the new cons. */
 	add x3, x0, #4			/* Point to the cdr of our extended list. */
	bl poplisp
	mov x7, x0
	b 10b

30:	bl getchar			/* Eat the dot. */
	mov x0, x7
	bl pushlisp
	bvs 999f
	bl read1			/* Read the cdr. */
	bvs 999f
	str x7, [ x3 ]			/* Store the cdr. */
	bl poplisp
	mov x7, x0

	bl skipwhite		 	/* Expect a closing parenthesis. */
	LOOK x0
	cmp x0, #')'
	beq 990f

	ERRSET errparenmissing
	b 999f

990:	bl getchar			/* Eat the closing parenthesis. */
	ERRCLR
999:	pop { x0-x3, pc }

	ENDPROC

	/*

	Read a symbol starting with the character in x0, returning a pointer to the
	symbol in x7.

	This procedure assumes that, on entry, the character in x0 has been validated by
	issym.

	*/
	PROC readsym

	push { x0-x2, lr }
	mov x1, #0			/* Number of characters in the symbol. */
	ldr x2, =internbuffer
1:	cmp x1, #INTERNMAX		/* A symbol can be longer than */
	bge 2f				/* INTERNMAX but we only recognize */
	strb x0, [ x2 ]			/* the first INTERNMAX characters. */
	add x2, x2, #1
	add x1, x1, #1
2:	bl getchar
	LOOK x0
	bl iswhite
	beq 3f
	cmp x0, #'('
	beq 3f
	cmp x0, #')'
	beq 3f
	cmp x0, #'.'
	beq 3f
	cmp x0, #EOT
	beq 990f

	bl issym
	beq 1b
	
	bl getchar			/* Eat the non-symbol character. */
	ERRSET errbadsym
	b 999f
	
3:	ldr x2, =internbufferlen	/* Finished reading.  Store the length. */
	str x1, [ x2 ]
	bl intern

990:	ERRCLR

999:	pop { x0-x2, pc }

	ENDPROC

	/*

	When the kernel hands the CPU over to an executable file, it calls _start.
	Normally the standard startup libary provides this entry point, so we need to
	furnish our own.

	*/

	.global _start

	PROC _start

	ldr x1, =greeting
	ldr x2, =GREETINGLEN
	bl write
	bl initfreelist
	b repl

	ENDPROC

	.data
	.balign 4
greeting:
	.ascii "arpilisp version 22/7\n\n"
	.equiv GREETINGLEN, . - greeting

	/*

	To terminate arpilisp, we use the sys_exit system call.  This call needs only one
	parameter, the exit status.  Unlike other system calls, the kernel doesn't
	return execution to arpilisp after we make the system call for sys_exit.  So we
	don't bother pushing and popping registers. 

	*/

	PROC finish

	mov x7, #1			/* sys_exit */
	mov x0, #0			/* Exit status. */
	svc #0				/* Call the system. */

	ENDPROC

	/*

	Panic when we reach an error condition that we can't recover from.  Print the
	symbol in x0 and return to the OS.

	*/

	PROC panic

	bl print
	b finish

	ENDPROC

	/*
	________________________________________________________________________________

	The Read Evaluate Print Loop

	Congratulations for getting through the low-level machinery of a Lisp
	interpreter!  Now we can implement higher-level parts of Lisp itself, stepping
	in hybrid territory between assembly and Lisp.

	The Read Evaluate Print Loop (REPL) is self-explanatory: read an S-expression,
	evaluate it, print the value, and do it again.

	The REPL implies direct interaction with the computer, which seems obvious
	today.  Surprise! This is another innovation that was practically science
	fiction in the 1950s and '60s.  Back then computers were rare and expensive
	enough that every minute of processing time was carefully accounted for.
	"Users" were only allowed to access computers via technicians and often weren't
	allowed to even be in the same room.

	Today, we interact with computers ubiquitously, to a level that we don't even
	call them computers; we call them phones, tablets, watches, cars, and other
	things.  Thank (or curse) Lisp for lighting the path that we took to get here.

	Notice that our REPL might more accurately be called RDorEPL; Read, Define or
	Evaluate-Print, Loop.  To simplify our interpreter, we separate variable
	definition from evaluation.

	*/

	PROC repl

	mov x9, #NIL			/* Start with an empty environment. */

	/* Reset the Lisp registers and Lisp stack. */
10:	ldr x6, =lispstacktop		/* Empty the Lisp stack. */
	mov x8, #NIL			/* The S-expression to evaluate. */
	mov x7, #NIL			/* The value of the evaluated expression. */
	mov x5, #NIL			/* The list of lambda argument values. */

	bl read
	bvs 30f
	mov x8, x7
	bl isdef
	bne 15f
	bl defsym
	bvs 30f
	b 10b
	
15:	bl eval
	bvs 30f
	mov x0, x7
	bl print
	b 10b

	/* Print an error. */
30:	mov x0, x7
	bl prin1
	mov x0, x8
	bl print
	b 10b

	ENDPROC

	/*

	In Lisp, the atom function returns t if its argument is nil or a symbol. It
	returns nil otherwise, which implies that its argument is a cons cell.

	Our assembly version sets the Z condition.
	
	*/
	PROC atom
	push { lr }

	cmp x0, #NIL
	beq 990f
	tst x0, #SYMMASK
	bne 990f

	ZCLR				/* Not an atom. */
	b 999f
	
990:	ZSET				/* We have an atom. */

999:	pop { pc }
	ENDPROC
	
	/*

	Car and cdr are simple in concept, but we implement versions that do some error
	checking.

	Return the car of x0 in x0.  If x0 is nil, return nil.  Otherwise, generate an
	error.

	*/

	PROC car

	push { lr }
	tst x0, #SYMMASK
	beq 1f
	ERRSET errcellornil
	b 999f

1:	cmp x0, #NIL
	beq 999f
	ldr x0, [ x0 ]
	ERRCLR

999:	pop { pc }

	ENDPROC

	PROC cdr

	push { lr }
	tst x0, #SYMMASK
	beq 1f
	ERRSET errcellornil
	b 999f

1:	cmp x0, #NIL
	beq 999f
	ldr x0, [ x0, #4 ]
	ERRCLR

999:	pop { pc }

	ENDPROC

	/*
	(define assoc
	  (lambda (key alist)
	    (cond ((null alist)               nil)
	  	  ((eq key (car (car alist))) (car alist))
		  (t                          (assoc key (cdr alist))))))
	
	*/

	PROC assoc

	push { x1-x2, lr }
	mov x1, x0			/* The key to search for. */
	mov x2, x9			/* Association list. */

1:	cmp x2, #NIL			/* At the end of the list? */
	bne 2f
	mov x0, #NIL
	b 999f

2:	mov x0, x2			/* Does the key match this pair? */
	bl car
	bl car
	cmp x1, x0
	bne 3f
	mov x0, x2
	bl car
	b 999f

3:	mov x0, x2			/* No match, try the next pair. */
	bl cdr
	mov x2, x0
	b 1b

999:	pop { x1-x2, pc }

	ENDPROC


	/*

	Determine if the expression pointed to by x8 is a define expression.  If so,
	then set the Z flag, return the symbol to define in x0, and return the
	expression to evaluate in x8.  If not, then clear the status registers.

	*/
	PROC isdef

	push { x1-x2, lr }

	mov x2, x0			/* Save x0. */

	mov x0, x8
	bl atom
	beq 990f
	
	bl car
	ldr x1, =symdefine
	cmp x0, x1
	bne 990f
	
	mov x0, x8			/* Get the symbol to bind. */
	bl cdr
	bvs 990
	bl car
	bvs 990
	mov x1, x0			/* Remember the symbol to bind. */
	
	mov x0, x8			/* Get the expression to evaluate. */
	bl cdr
	bvs 990
	bl cdr
	bvs 990
	bl car
	bvs 990
	bvs 990f
	mov x8, x0
	mov x0, x1
	ZSET				/* We have a define expression. */
	b 999f
	
990:	ZCLR				/* Not a define expression. */
	mov x0, x2			/* Restore x0. */
	
999:	pop { x1-x2, pc }
	ENDPROC
	
	/*

	The define function extends or modifies the environment.  It takes 2 arguments:
	a symbol and an expression to evaluate and bind to the symbol.

	Given a symbol in x0 and an expression in x7, and an environment in x9, evaluate
	the expression then create a binding in the environment.

	In the case of an error, store the offending expression in x8.

	*/

	PROC defsym

	push { x0-x2, lr }

	tst x0, #SYMMASK
	bne 5f
	mov x8, x0
	ERRSET errbindsym
	b 999f

5:	bl eval
	bvs 999f
	
	mov x2, x0			/* Remember our symbol. */

10:	bl assoc
	cmp x0, #NIL
	bne 20f

	/* Symbol is unbound, so bind it. */
	mov x1, x2
	mov x2, x7
	bl cons
	mov x1, x0
	mov x2, x9
	bl cons
	mov x9, x0
	b 990f

	/* Symbol is defined, change its value. */
20:	str x7, [ x0, #4 ]		/* Store it in the cdr. */

990:	ERRCLR
999:	pop { x0-x2, pc }

	ENDPROC


	
	/*


	(define eval
	  (lambda (expr env)
	    (cond
	     ((selfevalp expr)               expr)
	     ((eq expr (quote nil))          ())
	     ((symbolp expr)                 (cdr (assoc expr env)))
     	     ((eq (car expr) (quote quote))  (arg1 expr))
	     ((eq (car expr) (quote lambda)) expr)
	     ((eq (car expr) (quote atom))   (atom (eval (arg1 expr) env)))
	     ((eq (car expr) (quote car))    (car (eval (arg1 expr) env)))
	     ((eq (car expr) (quote cdr))    (cdr (eval (arg1 expr) env)))
	     ((eq (car expr) (quote eq))     (eq
					      (eval (arg1 expr) env)
					      (eval (arg2 expr) env)))
	     ((eq (car expr) (quote cons))   (cons
					      (eval (arg1 expr) env)
					      (eval (arg2 expr) env)))
	     ((eq (car expr) (quote cond))   (evalcond (cdr expr) env))
	     (t                              (apply
	                                      (eval (car expr) env)
	                                      (evlis (cdr expr) env) env)))))

	Before eval, let's define some helper functions and macros.

	*/

	.macro MATCHFORM sym
	mov x0, x8
	bl car
	ldr x1, =\sym
	cmp x0, x1
	.endm

	.macro ARG1
	mov x0, x8
	bl cdr
	blvc car
	.endm

	.macro ARG2
	mov x0, x8
	bl cdr
	blvc cdr
	blvc car
	.endm
	
	PROC selfevalp
	push { x1, lr }

	cmp x0, #NIL
	beq 999f

	ldr x1, =symt
	cmp x0, x1
	
999:	pop { x1, pc }
	ENDPROC
	
	/*

	Our ARM assembly eval takes an expression in x8 and an environment in x9,
	returning a value in x7.

	When we encounter an error, we try to leave the offending expression in x8.
	That means we don't pop x8 from the Lisp stack.  We can get away with this
	imbalanced push-pop because our REPL resets the Lisp stack for us.

	*/

	PROC eval
	push { x0-x2, lr }

	mov x0, x5
	bl pushlisp
	bvs 999f
	mov x0, x8
	bl pushlisp
	bvs 999f

	// ((selfevalp expr) expr)
	bl selfevalp
	bne 10f
	mov x7, x8
	b 990f

	// ((eq expr (quote nil)) ())
10:	ldr x1, =symnil
	cmp x0, x1
	bne 20f
	mov x7, #NIL
	b 990f

	// ((symbolp expr) (cdr (assoc expr env)))
20:	tst x0, #SYMMASK
	beq 40f
	bl assoc
	cmp x0, #NIL
	bne 30f
	ERRSET errunboundvar
	b 999f
	
30:	bl cdr
	bvs 999f
	mov x7, x0
	b 990f

40:	// ((eq (car expr) (quote quote)) (arg1 expr))
	MATCHFORM symquote
	bne 50f
	ARG1
	bvs 999f
	mov x7, x0
	b 990f

50:	// ((eq (car expr) (quote lambda)) expr)
	MATCHFORM symlambda
	bne 60f
	mov x7, x8
	b 990f

	// ((eq (car expr) (quote atom))   (atom (eval (arg1 expr) env)))
60:	MATCHFORM symatom
	bne 70f
	ARG1
	bvs 999f
	mov x8, x0
	bl eval
	bvs 999f
	mov x0, x7
	bl atom
	ldreq x7, =symt
	movne x7, #NIL
	b 990f

	// ((eq (car expr) (quote car))    (car (eval (arg1 expr) env)))
70:	MATCHFORM symcar
	bne 80f
	ARG1
	bvs 999f
	mov x8, x0
	bl eval
	bvs 999f
	mov x0, x7
	bl car
	bvs 999f
	mov x7, x0
	b 990f
	
	// ((eq (car expr) (quote cdr))    (cdr (eval (arg1 expr) env)))
80:	MATCHFORM symcdr
	bne 90f
	ARG1
	bvs 999f
	mov x8, x0
	bl eval
	bvs 999f
	mov x0, x7
	bl cdr
	bvs 999f
	mov x7, x0
	b 990f

	// ((eq (car expr) (quote eq))     (eq
	//				      (eval (arg1 expr) env)
	//				      (eval (arg2 expr) env)))
90:	MATCHFORM symeq
	bne 100f
	ARG1
	bvs 999f
	mov x1, x0			/* Unevaluated 1st argument. */
	ARG2
	bvs 999f
	mov x2, x0			/* Unevaluated 2nd argument. */
	mov x8, x1			/* Evaluate 1st argument. */
	bl eval
	bvs 999f
	mov x1, x7
	mov x8, x2			/* Evaluate 2nd argument. */
	bl eval
	bvs 999f
	mov x2, x7
	cmp x1, x2
	movne x7, #NIL
	ldreq x7, =symt
	b 990f

	// ((eq (car expr) (quote cons))     (cons
	//				      (eval (arg1 expr) env)
	//				      (eval (arg2 expr) env)))
100:	MATCHFORM symcons
	bne 110f
	ARG1
	bvs 999f
	mov x1, x0			/* Unevaluated 1st argument. */
	ARG2
	bvs 999f
	mov x2, x0			/* Unevaluated 2nd argument. */
	mov x8, x1			/* Evaluate 1st argument. */
	bl eval
	bvs 999f
	mov x1, x7
	mov x8, x2			/* Evaluate 2nd argument. */
	bl eval
	bvs 999f
	mov x2, x7
	bl cons
	mov x7, x0
	b 990f

	// ((eq (car expr) (quote cond))   (evalcond (cdr expr) env))
110:	MATCHFORM symcond
	bne 120f
	mov x0, x8
	bl cdr
	bvs 999f
	bl evalcond
	bvs 999f
	b 990f

	// (t (apply (eval (car expr) env) (evlis (cdr expr) env) env)))))
120:	mov x0, x8
	bl car
	bvs 999f
	mov x1, x0			/* Unevaluated function. */

	mov x0, x8
	bl cdr
	bvs 999f
	mov x2, x0			/* Unevaluated arguments. */

	mov x8, x1
	bl eval
	bvs 999f
	mov x1, x7			/* Evaluated function. */

	mov x0, x2
	bl evlis
	bvs 999f
	
	mov x0, x1
	bl apply
	bvs 999f

990:	bl poplisp
	mov x8, x0
	bl poplisp
	mov x5, x0
	ERRCLR
	
999:	pop { x0-x2, pc }
	ENDPROC

	/*

	More helper functions.

	First, let's implement cond.  This assembly procedure implements this Lisp
	function:

	(define evalcond (lambda (c env)
	  (cond ((eq c nil) nil)
		((eval (car (car c)) env) (eval (car (cdr (car c))) env))
		(t (evalcond (cdr c) env)))))

	Given a list of cond clauses in x0, return the value of the first clause that
	tests true in x7.

	*/

	PROC evalcond

	push { x0-x1, x8, lr }

	mov x1, x0

	// ((eq c nil) nil)
10:	cmp x0, #NIL
	moveq x7, x0
	beq 990f

	// ((eval (caar c) env) ...
	bl car
	blvc car
	mov x8, x0
	bl eval
	bvs 999f
	cmp x7, #NIL
	beq 20f

	// ... (eval (cadar c) env))
	mov x0, x1
	bl car
	blvc cdr
	blvc car
	bvs 999f
	mov x8, x0
	bl eval
	bvs 999f
	b 990f

	// (t (evalcond (cdr c) env))))
20:	mov x0, x1
	bl cdr
	bvs 999f
	mov x1, x0
	b 10b


990:	ERRCLR
999:	pop { x0-x1, x8, pc }

	ENDPROC

	/*



	(define evlis
	  (lambda (exprs env)
	    (cond ((null exprs) nil)
	          (t (cons
	               (eval (car exprs) env)
	               (evlis (cdr exprs) env))))))

	Given a list of unevaluated expressions in x0 and an environment pointed to by
	x9, return a corresponding list of evaluated values in x5. The evaluated values
	are in the same order as the values in the unevaluated list.

	*/

	PROC evlis

	push { x0-x4, x8, lr }

	mov x5, #NIL			/* Head of the values list. */
	mov x3, #NIL			/* End of the values list. */
	mov x4, x0			/* Remaining unevaluated expressions. */

	cmp x0, #NIL			/* Any expressions? */
	beq 990f

	bl car				/* Evaluate the first expression. */
	mov x8, x0
	bl eval
	bvs 999f

	mov x1, x7 			/* Start our list of values. */
	mov x2, #NIL
	bl cons
	mov x5, x0
	mov x3, x0
	mov x0, x4
	bl cdr
	mov x4, x0

10:	cmp x0, #NIL			/* Any more expressions? */
	beq 990f

	bl car
	bvs 999f
	mov x8, x0
	bl eval
	bvs 999f

	mov x1, x7
	mov x2, #NIL
	bl cons
	str x0, [ x3, #4 ]
	mov x3, x0

	mov x0, x4
	bl cdr
	mov x4, x0

	b 10b

990:	ERRCLR
999:	pop { x0-x4, x8, pc }

	ENDPROC

	/*
	(define apply
	  (lambda (fn args env)
	    (cond
	      ((null (cdr (cdr fn))) nil)
	      (t (apply-body (cdr (cdr fn)) (pairlis (car (cdr fn)) args env))))))
	
	(define apply-body
	  (lambda (body env)
	    (cond
	      ((null (cdr body)) (eval (car body) env))
	      (t (apply-next body env)))))

	(define apply-next
	  (lambda (body env)
	    (eval (car body) env)
	    (apply-body (cdr body) env)))

	Ironically, the Lisp definition for apply seems more complicated than its
	assembler equivalent below.  This difference is mostly due to Lisp's coziness to
	functional purity.  That's another way of saying, for our purposes anyway, that
	Lisp encourages us to avoid storing things in global variables.

	Assembly language offers almost nothing beyond the ability to store things in
	global variables.  This happens to be beneficial here, letting us take a more
	direct route to implement the logic of apply.  In our case, we have x7, which
	contains the value of the most recently evaluated expression.

	For what it's worth, a more sophisticated Lisp would allow us to define a much
	shorter, single-function apply.

	Given a lambda in x0, a list of values in x5, and an environment in x9, bind
	each lambda argument to its value then evaluate each of the expressions in the
	lambda body, returning the value of the last expression in x7.  If the body is
	empty, return nil.

	We need to be careful about x9, our environment.  If there's an error during the
	evaluation of the lambda body, we need to restore x9 to the pre-apply
	environment.

	*/

	PROC apply

	push { x0-x2, x8-x9, lr }

	mov x2, x0			/* Remember our function. */

	/* Make sure we have a lambda. */
	bl car
	bvs 980f
	
	ldr x1, =symlambda
	cmp x0, x1
	bne 980f

	mov x7, #NIL			/* Assume an empty body. */

	mov x0, x2
	bl cdr				/* Skip the lambda symbol. */

	mov x2, x0			/* Bind the parameters. */
	bl car
	bl pairlis
	bvs 999f

	mov x0, x2			/* Skip the parameter list. */
	bl cdr
	mov x2, x0

	/* Apply the body. */
10:	cmp x0, #NIL			/* Any more expressions? */
	beq 990f
	bl car				/* Evaluate the next expression. */
	bvs 999f
	mov x8, x0
	bl eval
	bvs 999f			/* Give up if there's an error. */
	mov x0, x2
	bl cdr
	mov x2, x0
	b 10b

980:	ERRSET errbadlistexpr
	b 999f

990:	ERRCLR
999:	pop { x0-x2, x8-x9, pc }

	ENDPROC

	/*

	We need a way to bind a lambda's parameters to values then extend the
	environment with these bindings.  As we saw earlier, lambda parameters are
	independent of existing bindings of the same symbols.

	The arguments for a lambda application are evaluated in the environment outside
	of the lambda.

	For example:

	(define x (quote marks-the-spot))

	((lambda (x) (null x)) x)

	nil

	Redefining x gives a different result:

	(define x nil)

	((lambda (x) (null x)) x)
	t

	A lambda can also use a binding that exists before the lambda is applied.  For
	example:

	(define name (quote Nicole))

	((lambda (x) (eq name x)) (quote Nicole))
	t

	For arpilisp, we implement this:

	(define pairlis
	  (lambda (params vals env)
	    (cond
	      ((null params) (cond ((null vals) env) (t (quote errargextra))))
	      ((null vals) (quote errargmissing))
	      (t (cons
	           (cons (car params) (car vals))
	           (pairlis (cdr params) (cdr vals) env))))))
  
	Given a list of parameters in x0, a list of values in x5, and an environment to
	extend in x9, bind each parameter to a value then extend x9.

	*/

	PROC pairlis

	push { x0-x3, lr }

	mov x3, x0			/* List of parameters. */

5:	cmp x3, #NIL			/* Are parameters and values empty? */
	bne 20f
	cmp x5, #NIL
	beq 990f

10:	cmp x3, #NIL
	bne 20f
	ERRSET errargextra
	b 999f

20:	cmp x5, #NIL
	bne 30f
	ERRSET errargmissing
	b 999f

30:	mov x0, x3
	bl car
	mov x1, x0
	mov x0, x5
	bl car
	mov x2, x0
	bl cons
	mov x1, x0
	mov x2, x9
	bl cons
	mov x9, x0
	mov x0, x3
	bl cdr
	mov x3, x0
	mov x0, x5
	bl cdr

	mov x5, x0
	b 5b

990:	ERRCLR
999:	pop { x0-x3, pc }

	ENDPROC

	/*
	________________________________________________________________________________

	The End

	And the beginning.  Arpilisp's eval provides the core of a Lisp interpreter.  You
	can use arpilisp to implement the rest of a pretty capable Lisp system.

	To start, you can add some traditional utility functions like caar, cadr,
	cdar, cddr.  You can add some powerful function application functions like
	map.  And of course, you can extend eval by writing your own.

	If you store the rest of your Lisp in a file named my.lisp, you can load
	it and continue in the REPL with this shell command:

	cat my.lisp - | ./arpilisp
	
