BITS 16
org 0x0500

; dx = A register
; bp = PSP (parameter stack pointer)
; sp = RSP (return stack pointer) ; FORTH MACROS

; USEFUL MACROS

%macro	spush 1 ; src
	sub	bp, 2
	mov	word [bp], %1
%endmacro

%macro	spop 1 ; dst
	mov	word %1, [bp]
	add	bp, 2
%endmacro

%macro	litb	1
	mov	byte [di], %1
	inc	di
%endmacro

%macro	litw	1
	mov	word [di], %1
	add	di, 2
%endmacro

; WORD STRUCTURE

%macro wordinit 2
	db	%1	; name (n bytes)
	dw	0	; link (2 bytes)
	db	%2	; len  (1 byte)
	%push dict
	%$link:
%endmacro

%macro wordlink 2
	db	%1	; name (n bytes)
	dw	%$link	; link (2 bytes)
	db	%2	; len  (1 byte)
	%pop
	%push dict
	%$link:
%endmacro		; load (n bytes)

%define FLAG_IMMEDIATE	0x80	; immediate flag
%define MASK_LENGTH	0x7f	; namelen mask
%define BOOTDRIVE	7c3eh

%macro	const 3 ; name, len, lit
	wordlink %1, %2
	spush	%3
	ret
%endmacro

start:		mov	bp, 0xff00	; initialize SP
		mov	sp, 0x0000	; initialize RS
		jmp	interpret

; SYSTEM VARIABLES

state:		db 0	; interpret or compile mode

latest:		dw noop	; last word of the dictionary
here:		dw end	; next available space in dictionary

in:		dw buffer	; current char read in buffer
buffer:		times 65 db 0	; input buffer

curchar:	dw 0	; current word addr
curlen:		db 0	; current word len

blk:		dw 0x7e00	; pointer to current line
line:		dw 16*3		; number of lines to read

; THE DICTIONARY
		; key ( -- c )
		; fetch char c from direct input
		wordinit 'key', 3
key:		mov	ah, 0x00
		int	0x16
		spush	ax	; ah: scan code, al: ascii char
		ret

		; emit ( c -- )
		; spit char c to output stream
		wordlink 'emit', 4
emit:		spop	ax
		mov	ah, 0x0e
		int	0x10
		ret

		; abort ( -- )
		; reset PS and jump to quit
		wordlink 'abort', 5
abort:		mov	bp, 0xff00
		jmp	quit

		; quit ( -- )
		; reset RS and return to interpreter prompt
		wordlink 'quit', 4
quit:		mov	sp, 0x0000
		mov	byte [state], 0
		mov	word [line], 0
		mov	word [in], buffer+64
		jmp	interpret

		; interpret ( -- )
		; main interpret loop
		wordlink 'interpret', 9

interpret:	; read next word
		call	toword		; ( -- sa sl )

		; is a number?
		call	parse		; ( sa sl -- n? f )
		spop	ax		; ( n? f -- n? )
		test	ax, ax
		jnz	.num

		; is a word?
		call	curword		; ( -- sa sl )
		call	find		; ( sa sl -- w? f )
		spop	ax		; ( w? f -- w? )
		test	ax, ax
		jnz	.word

		; not a word
		jmp	notfound

	.num:	mov	al, [state]	; ( n -- n )
		test	al, al
		jz	interpret

		call	litn		; ( n -- )
		jmp	interpret

	.word:	mov	al, [state]
		test	al, al
		jz	.run

		; immediate word?
		mov	bx, [bp]
		mov	al, [bx-1]
		and	al, FLAG_IMMEDIATE
		test	al, al
		jnz	.run

		call	compile_comma	; ( w -- )
		jmp	interpret	; ( -- )

	.run:	call	execute		; ( w -- )
		jmp	interpret	; ( -- )

notfound:	call	nl_out		; ( -- )
		call	curword		; ( -- sa sl )
		call	type		; ( sa sl -- )
		spush	.err		; ( -- sa )
		spush	15		; ( -- sa sl )
		call	type		; ( sa sl -- )
		jmp	abort

	.err:	db ' word not found'

notnumber:	call	nl_out
		call	curword
		call	type
		spush	.err		; ( -- sa )
		spush	13		; ( -- sa sl )
		call	type		; ( sa sl -- )
		jmp	abort

	.err:	db ' not a number'

underflow:	spush	.err		; ( -- sa )
		spush	21		; ( -- sa sl )
		call	type		; ( sa sl -- )
		jmp	abort

	.err:	db 0x0d, 0x0a, 'stack out of bounds'

		; type ( sa sl -- )
		; emit all chars of string
		wordlink 'type', 4
type:		spop	cx
		spop	si
		mov	ah, 0x0e
	.next:	lodsb
		int	0x10
		loop	.next
		ret

		; word ( -- sa sl )
		; read one word from buffered input
		; update curchar and curlen
		wordlink 'word', 4
toword:		call	tochar
		spop	ax
		cmp	ax, 0x20
		jbe	toword
		mov	ax, [in]
		dec	ax
		spush	ax
		mov	[curchar], ax
		xor	cx, cx

	.next:	inc	cx
		call	tochar
		spop	ax
		cmp	ax, 0x20
		ja	.next

		spush	cx
		mov	[curlen], cl
		ret

		; curword ( -- sa sl )
		; yield the last read word
		wordlink 'curword', 7
curword:	mov	ax, [curchar]
		spush	ax
		mov	al, [curlen]
		xor	ah, ah
		spush	ax
		ret

		; in< ( -- c )
		; read one char from buffered input
		; if end of line, read new line
		wordlink 'in<', 3
tochar:		mov	bx, [in]
		cmp	bx, buffer+64
		jbe	.skip
		call	ln_in
		mov	bx, buffer
	.skip:	mov	al, [bx]
		xor	ah, ah
		spush	ax
		inc	bx
		mov	[in], bx
		ret

		; ln< ( -- )
		; routine that feeds lines to the interpreter
		wordlink 'ln<', 3
ln_in:		call	scnt
		spop	ax
		test	ax, ax
		js	underflow
		mov	ax, [line]
		test	ax, ax
		jz	rdln
		dec	ax
		mov	[line], ax
		jmp	rdbln

		; read block line
		wordlink 'rdbln', 5
rdbln:		mov	si, [blk]
		mov	di, buffer
		mov	cx, 64
		rep	movsb
		litb	0
		mov	word [blk], si
		mov	word [in], buffer
		ret

		; rdln ( -- )
		; feed a line to the buffered input
		wordlink 'rdln', 4
rdln:		spush	.ok
		spush	5
		call	type
		mov	di, buffer

	.next	cmp	di, buffer+64	; buffer full?
		je	.cr		; submit
		mov	ah, 0x00	; call key
		int	0x16
		cmp	al, 0x0d	; submit?
		je	.cr
		cmp	al, 0x08	; backspace?
		je	.bs
		cmp	al, 0x20	; control character?
		jb	.next		; do nothing

		stosb			; store string byte
		mov	ah, 0x0e	; emit char
		int	0x10
		jmp	.next

	.bs:	cmp	di, buffer	; empty buffer?
		je	.next		; do nothing
		dec	di		; clear previous char
		mov	byte [di], 0
		call	bs_out
		jmp	.next

	.cr:	call	spc_out		; emit space after enter
		xor	al, al		; flush rest of line
	.null:	stosb			
		cmp	di, buffer+64
		jbe	.null
		mov	word [in], buffer
		ret

	.ok	db ' ok', 0x0d, 0x0a ; CR, LF

		; parse ( sa sl -- n? f )
		; convert string as a number
		wordlink 'parse', 5
parse:		spop	cx
		spop	si
		xor	ax, ax	; current char
		xor	bx, bx	; result

		lodsb
		dec	cx
		cmp	al, "'"
		je	.char
		cmp	al, '$'
		je	.hex
		cmp	al, '-'
		je	.neg
		inc	cx
		jmp	.skip

		; character literal
	.char:	cmp	cx, 2
		jne	.nan
		lodsb
		mov	bl, al
		lodsb
		cmp	al, "'"
		jne	.nan
		jmp	.num

		; hexadecimal
	.hex:	lodsb
		shl	bx, 4		; multiply by 16
		cmp	al, '0'
		jb	.nan
		cmp	al, '9'
		jbe	.digit
		cmp	al, 'a'
		jb	.nan
		cmp	al, 'f'
		jbe	.alpha
		jmp	.nan
	.digit: sub	al, '0'
		jmp	.add
	.alpha: sub	al, 'a'-10
	.add:	add	bx, ax
		loop	.hex
		jmp	.num

		; negative decimal
	.neg:	lodsb
		imul	bx, 10
		sub	al, '0'
		jb	.nan
		cmp	al, 10
		jae	.nan
		sub	bx, ax
		loop	.neg
		jmp	.num

		; unsigned decimal
	.pos:	lodsb
		imul	bx, 10
	.skip:	sub	al, '0'
		jb	.nan
		cmp	al, 10
		jae	.nan
		add	bx, ax
		loop	.pos

	.num:	spush	bx
		spush	-1
		ret

	.nan:	spush	0
		ret

		; lit ( -- n ) runtime
		wordlink 'lit', 3
lit:		pop	si
		mov	word di, [si]
		add	si, 2
		spush	di
		push	si
		ret

		; litn ( n -- )
		; compile n as a literal
		wordlink 'litn', 4
litn:		mov	di, [here]
		mov	byte [di], 0xe8
		inc	di
		mov	si, lit		; relative addressing
		sub	si, di
		sub	si, 2
		mov	word [di], si
		add	di, 2
		spop	si		; store n
		mov	word [di], si
		add	di, 2
		mov	[here], di
		ret

; 		wordlink 'litn', 4
; litn:		mov	di, [here]
; 		litb	0x83		; sub bp, 2
; 		litb	0xed
; 		litb	0x02
; 		litb	0xc7		; mov [bp], imm16
; 		litb	0x46
; 		litb	0x00
; 		mov	word si, [bp]
; 		mov	[di], si
; 		add	di, 2
; 		add	bp, 2
; 		mov	[here], di
; 		ret

		; find ( sa sl -- w? f )
		; search for counted string in dictionary
		wordlink 'find', 4
find:		spop	ax		; len
		spop	dx		; addr
		mov	bx, [latest]	; curr

	.loop:	mov	cl, [bx-1]	; same length?
		and	cl, 0x7f	; ignore immediate flag
		cmp	al, cl
		jne	.skip

		mov	si, dx
		mov	di, bx
		sub	di, 3
		sub	di, ax		; beginning of name
		mov	cx, ax
		repz	cmpsb
		jz	.found	

	.skip:	mov	bx, [bx-3]	; next entry
		cmp	bx, 0
		jnz	.loop

		spush	0
		ret

	.found: spush	bx
		spush	-1
		ret

		; execute ( w -- )
		; jump to addr w
		wordlink 'execute', 7
execute:	spop	ax
		jmp	ax

		; compile, ( w -- )
		; append a call to wordref w to here
		wordlink 'compile,', 8
compile_comma:	mov	di, [here]
		litb	0xe8 ; call opcode
		spop	ax ; wordref
		sub	ax, di
		sub	ax, 2
		stosw
		mov	[here], di
		ret

		; header ( sa sl -- )
		; append a new header with name s
		wordlink 'header', 6
header:		spop	bx
		mov	cx, bx
		spop	si
		mov	di, [here]
		rep	movsb
		mov	ax, [latest]
		stosw
		mov	al, bl
		stosb
		mov	[latest], di
		mov	[here], di
		ret

		; create x ( -- )
		; append a new header with name x
		wordlink 'create', 6
create:		call	toword		; ( -- sa sl )
		call	header		; ( sa sl -- )
		ret

		; immediate ( -- )
		; make latest word an immediate word
		wordlink 'immediate', 9
immediate:	mov	bx, [latest]
		dec	bx
		mov	al, [bx]
		xor	al, FLAG_IMMEDIATE
		mov	[bx], al
		ret

		; [ ( -- ) IMMEDIATE
		; to immediate mode
		wordlink '[', 1 | FLAG_IMMEDIATE
to_immediate:	mov	byte [state], 0
		ret

		; ] ( -- )
		; to compile mode
		wordlink ']', 1
to_compile:	mov	byte [state], -1
		ret

		; exit ( -- )
		; compile exit from a word
		wordlink 'exit', 4 | FLAG_IMMEDIATE
exit:		mov	di, [here]
		litb	0xc3 ; ret
		mov	[here], di
		ret

		; : ... ( -- )
		; create a new word definition with name ...
		wordlink ':', 1
colon:		call	create
		call	to_compile
		ret

		; ; ( -- )
		; end current word definition
		wordlink ';', 1 | FLAG_IMMEDIATE
semicolon:	call	exit
		call	to_immediate
		ret

		; spc> ( -- )
		; emit space character
		wordlink 'spc>', 4
spc_out:	spush	0x20 ; SP
		call	emit
		ret

		; nl> ( -- )
		; emit newline
		wordlink 'nl>', 3
nl_out:		spush	.nl
		spush	2
		call	type
		ret

	.nl:	db 0x0d, 0x0a ; CR, LF

		; bs> ( -- )
		; delete prev char in TTY mode
		wordlink 'bs>', 3
bs_out:		spush	.bs
		spush	3
		call	type
		ret

	.bs:	db 0x08, 0x20, 0x08 ; BS, SPC, BS

; FLOW

		; ( -- )
		; ignore input until ')' is read
		wordlink '(', 1 | FLAG_IMMEDIATE
paren:		call	tochar
		spop	ax
		cmp	ax, ')'
		jne	paren
		ret

		; ( -- )
		; ignore input until end of line
		wordlink '\', 1 | FLAG_IMMEDIATE
backslash:	mov	word [in], buffer+64
		ret

		; abort" ..." ( -- )
		; compiles a ." followed by an abort"
		wordlink 'abort"', 6 | FLAG_IMMEDIATE
abort_quote:	call	dot_quote
		spush	abort
		call	compile_comma
		ret

		wordlink 'if', 2 | FLAG_IMMEDIATE
if:		spush	.if
		call	compile_comma
		mov	di, [here]
		litb	0xe9		; jmp rel16
		spush	di
		add	di, 2
		mov	[here], di
		ret

	.if:	spop	ax
		test	ax, ax
		jz	.jump
		pop	ax
		add	ax, 3
		jmp	ax
	.jump	ret
		

		; compile-time	( a1 -- a2 )
		; run-time	( -- )
		wordlink 'else', 4 | FLAG_IMMEDIATE
else:		mov	di, [here]	; compile
		litb	0xe9		; jmp rel16
		spush	di
		add	di, 2		; leave 2 byte space
		mov	[here], di
		call	swap
		jmp	then		; compile rel16 to 'if'

		; compile-time	( a -- )
		; run-time	( -- )
		wordlink 'then', 4 | FLAG_IMMEDIATE
then:		spop	bx
		mov	ax, [here]
		sub	ax, bx
		sub	ax, 2
		mov	[bx], ax
		ret

		; compile-time	( -- a )
		; run-time	( -- )
		wordlink 'begin', 5 | FLAG_IMMEDIATE
begin:		mov	ax, [here]
		spush	ax
		ret

		; compile-time	( a -- )
		; run-time	( -- )
		wordlink 'again', 5 | FLAG_IMMEDIATE
again:		mov	di, [here]
		litb	0xe9		; jmp xxx
		spop	ax
		sub	ax, di
		sub	ax, 2
		stosw
		mov	[here], di
		ret

		; compile-time	( a -- )
		; run-time	( f -- )
		wordlink 'until', 5 | FLAG_IMMEDIATE
until:		spush	.until
		call	compile_comma
		jmp	again

	.until:	spop	ax
		test	ax, ax
		jz	.loop
		pop	ax	; skip jmp rel16 (3 bytes)
		add	ax, 3
		jmp	ax
	.loop	ret

		; compile-time	( a -- )
		; run-time	( R: n -- )
		wordlink 'next', 4 | FLAG_IMMEDIATE
next:		spush	.next
		call	compile_comma
		jmp	again

	.next:	pop	bx
		pop	ax
		dec	ax
		push	ax
		test	ax, ax
		jnz	.loop
		pop	ax
		add	bx, 3
	.loop:	jmp	bx

		; leave ( -- )
		; in a begin..next loop, exit at the next jump
		wordlink 'leave', 5
leave:		pop	bx
		pop	ax
		push	1
		push	bx
		ret

		; [if] ( f -- )
		; works outside of definitions
		; all [if] leads to the first [then]
		wordlink '[if]', 4
meta_if:	spop	ax
		test	ax, ax
		jz	.jump
		ret
	.jump:	call	toword
		spop	ax
		cmp	ax, 6
		je	.next
		add	bp, 2 ; drop
		jmp	.jump
	.next:	spush	.then
		spush	ax
		call	scmp
		spop	ax
		test	ax, ax
		jz	.jump
		ret
	.then:	db '[then]'

		; [then] ( -- )
		; reference for [if], on its own does nothing
		wordlink '[then]', 6
meta_then:	ret
		

; SYSTEM VARIABLES

; ( -- a ) currently selected line
const 'blk', 3, blk

; ( -- a ) current line count
const 'line', 4, line

; ( -- a ) last word of the directionary
const 'latest', 6, latest

; ( -- a ) next available space in dictionary
const 'here', 4, here

; ( -- a ) beginning of the input buffer
const 'in(', 3, buffer

; ( -- a ) end of the input buffer
const 'in)', 3, buffer+64

; ( -- a ) current position in the input buffer
const 'in>', 3, in

; ENTRY MANAGEMENT

		; '? ... ( -- f )
		; find ... in dictionary, return true if found
		wordlink "'?", 2
tick?:		call	toword		; ( -- sa sl )
		call	find		; ( sa sl -- w? f )
		mov	ax, [bp]
		test	ax, ax
		jz	.done
		call	nip
	.done:	ret

		; ' ... ( -- w )
		; find addr of word ..., abort if fail
		wordlink "'", 1
tickw:		call	toword		; ( -- sa sl )
		call	find		; ( sa sl -- w? f )
		spop	ax
		test	ax, ax
		jz	notfound
		ret

		; ['] ... runtime ( -- w )
		; similar to "'", but store addr as a literal
		wordlink "[']", 3 | FLAG_IMMEDIATE
tickimm:	call	toword		; ( -- sa sl )
		call	find		; ( sa sl -- w? f )
		spop	ax
		test	ax, ax
		jz	notfound
		call	litn		; ( w -- )
		ret

		; forget ... ( -- )
		; rewind dict up to ...'s prev entry
		wordlink "forget", 6
forget:		call	toword
		call	find
		spop	ax
		test	ax, ax
		jz	notfound
		spop	bx
		mov	ax, [bx-3]	; next entry
		mov	word [latest], ax
		mov	al, [bx-1]
		and	al, MASK_LENGTH
		xor	ah, ah
		sub	bx, ax
		sub	bx, 3
		mov	word [here], bx
		ret

; DEFINING WORDS

		; value ... ( n -- )
		; create cell ... that returns its value
		wordlink 'value', 5
value:		call	create
		call	litn
		call	exit
		ret

		; to ... ( n -- )
		; reassign value of ... to n
		wordlink 'to', 2 | FLAG_IMMEDIATE
to_value:	call	toword
		call	find
		spop	ax
		test	ax, ax
		jz	notfound
		mov	al, [state]
		test	al, al
		jz	.imm
		call	litn
		spush	.imm
		call	compile_comma
		ret
	.imm:	spop	bx ; wordref
		spop	ax ; number
		add	bx, 3
		mov	[bx], ax
		ret

		; alias x y ( -- )
		; define an alias y to x
		wordlink 'alias', 5
alias:		call	toword
		call	find
		spop	ax
		test	ax, ax
		jz	notfound
		call	create
		mov	di, [here]
		litb	0xe9		; jmp rel16
		spop	ax ; wordref
		sub	ax, di
		sub	ax, 2
		stosw
		mov	[here], di
		ret

		; [compile] ... ( -- )
		; compile word ... and write it to here
		wordlink '[compile]', 9 | FLAG_IMMEDIATE
compile_imm:	call	tickw
		call	compile_comma
		ret


; ASCII CONSTANTS

const 'BS', 2, 0x08	; backspace
const 'CR', 2, 0x0d	; carriage return
const 'LF', 2, 0x0a	; line feed
const 'SPC', 3, 0x20	; space character

; PARAMETER STACK

		; ( a -- )
		wordlink 'drop', 4
drop:		add	bp, 2
		ret

		; ( a -- a a )
		wordlink 'dup', 3
dup:		mov	ax, [bp]
		spush	ax
		ret

		; ( a -- a a? )
		; dup if a is nonzero
		wordlink '?dup', 4
?dup:		mov	ax, [bp]
		test	ax, ax
		jz	.zero
		spush	ax
	.zero:	ret

		; ( a b -- b )
		wordlink 'nip', 3
nip:		spop	ax
		mov	[bp], ax
		ret

		; ( a b -- a b a )
		wordlink 'over', 4
over:		mov	bx, bp
		add	bx, 2
		mov	bx, [bx]
		spush	bx
		ret

		; ( a b c -- b c a )
		wordlink 'rot', 3
rot:		spop	ax
		spop	bx
		mov	cx, [bp]
		mov	[bp], bx
		spush	ax
		spush	cx
		ret

		; ( a b c -- c a b )
		wordlink 'nrot', 4
nrot:		spop	ax
		spop	bx
		mov	cx, [bp]
		mov	[bp], ax
		spush	cx
		spush	bx
		ret

		; ( a b -- b a )
		wordlink 'swap', 4
swap:		spop	ax
		mov	bx, [bp]
		mov	[bp], ax
		spush	bx
		ret

		; ( a b -- b a b )
		wordlink 'tuck', 4
tuck:		spop	ax
		mov	bx, [bp]
		mov	[bp], ax
		spush	bx
		spush	ax
		ret

		; ( a a -- )
		wordlink '2drop', 5
twodrop:	add	bp, 4
		ret

		; ( a b -- a b a b )
		wordlink '2dup', 4
twodup:		spop	ax
		mov	bx, [bp]
		sub	bp, 4
		mov	[bp], bx
		spush	ax
		ret


; RETURN STACK

		; ( n -- R:n )
		wordlink '>r', 2
to_r:		spop	ax
		pop	bx
		push	ax
		push	bx
		ret

		; ( R:n -- n )
		wordlink 'r>', 2
r_from:		pop	ax
		pop	bx
		push	ax
		spush	bx
		ret

		; ( R:n -- n R:n )
		wordlink 'r@', 2
r_fetch:	pop	ax
		pop	bx
		spush	bx
		push	bx
		push	ax
		ret

		; ( R:n -- )
		wordlink 'rdrop', 5
rdrop:		pop	ax
		pop	bx
		push	ax
		ret

; STACK META

		; .s ( -- )
		; prints contents of the stack
		wordlink '.s', 2
dot_s:		call	scnt
		call	shiftr

		spush	'<'
		call	emit
		call	dup
		call	dot
		spush	'>'
		call	emit

		spop	cx
		mov	si, 0xfefe
		std

	.next:	test	cx, cx
		jz	.done
		dec	cx
		lodsw
		spush	ax
		call	spc_out
		call	dot
		jmp	.next

	.done:	cld
		ret

		; scnt ( -- n )
		; size of PS in bytes
		wordlink 'scnt', 4
scnt:		mov	ax, 0xff00
		sub	ax, bp
		spush	ax
		ret

		; rcnt ( -- n )
		; size of RS in bytes
		wordlink 'rcnt', 4
rcnt:		mov	ax, 0x0000
		sub	ax, sp
		spush	ax
		ret


; MEMORY

		; ( a -- n )
		; fetch value stored at addr a
		wordlink '@', 1
fetch:		mov	bx, [bp]
		mov	bx, [bx]
		mov	[bp], bx
		ret

		; ( n a -- )
		; store n at addr a
		wordlink '!', 1
store:		spop	bx
		spop	ax
		mov	[bx], ax
		ret

		; ( n -- )
		; write n in here and advance it
		wordlink ',', 1
comma:		spop	ax
		mov	di, [here]
		stosw
		mov	[here], di
		ret

		; ( n a -- )
		; increase value at addr a by n
		wordlink '+!', 2
plus_store:	spop	bx
		spop	ax
		add	[bx], ax
		ret

		; ( a1 a2 u -- f )
		; compare u bytes between a1 and a2
		wordlink '[]=', 3
scmp:		spop	cx
		spop	si
		spop	di
		repe	cmpsb
		jz	.true
		spush	0
		ret
	.true:	spush	-1
		ret

		; ( c a u -- i )
		; look for c within u bytes at addr a
		wordlink '[c]?', 4
c_find:		spop	cx
		spop	si
		spop	ax
		xor	bx, bx
	.next	cmp	[si], al
		je	.true
		inc	bx
		inc	si
		loop	.next
		spush	-1
		ret
	.true	spush	bx
		ret

		; ( a -- c )
		; fetch byte c stored at addr a
		wordlink 'c@', 2
c_fetch:	mov	bx, [bp]
		mov	bl, [bx]
		xor	bh, bh
		mov	[bp], bx
		ret

		; ( a -- a+1 c )
		; fetch byte c stored at addr a and inc a
		wordlink 'c@+', 3
c_fetch_plus:	mov	bx, [bp]
		mov	al, [bx]
		xor	ah, ah
		inc	bx
		mov	[bp], bx
		spush	ax
		ret

		; ( c a -- )
		; store byte c in addr a
		wordlink 'c!', 2
c_store:	spop	bx
		spop	ax
		mov	[bx], al
		ret

		; ( c a -- a+1 )
		; store byte c in addr a and inc a
		wordlink 'c!+', 3
c_store_plus:	spop	bx
		mov	ax, [bp]
		mov	[bx], al
		inc	bx
		mov	[bp], bx
		ret

		; ( c -- )
		; store byte c in here and advance it
		wordlink 'c,', 2
c_comma:	spop	ax
		mov	bx, [here]
		mov	[bx], al
		inc	bx
		mov	[here], bx
		ret

		; ( n -- )
		; move here by n bytes
		wordlink 'allot', 5
allot:		spop	ax
		mov	bx, [here]
		add	bx, ax
		mov	word [here], bx
		ret

		; ( n -- )
		; allot n bytes and fill with zero
		wordlink 'allot0', 6
allot0:		spop	cx
		mov	di, [here]
		xor	ax, ax
		rep	stosb
		mov	[here], di
		ret

		; ( a n c -- )
		; fill n bytes at addr a with char c
		wordlink 'fill', 4
fill:		spop	ax
		spop	cx
		spop	di
		rep	stosb
		ret

		; ( a1 a2 u -- )
		; copy u bytes from a1 to a2
		wordlink 'move', 4
move:		spop	cx
		spop	di
		spop	si
		rep	movsb
		ret

		; ( a u -- )
		; copy u bytes from a to here
		wordlink 'move,', 5
move_comma:	spop	cx
		spop	si
		mov	di, [here]
		rep	movsb
		mov	[here], di
		ret

; A REGISTER

		; ( n -- A:n )
		wordlink '>a', 2
to_a:		spop dx
		ret

		; ( A:n -- n )
		wordlink 'a>', 2
a_from:		spush dx
		ret

		; ( R:n -- A:n )
		wordlink 'r>a', 3
r_to_a:		pop	ax
		pop	dx
		jmp	ax

		; ( A:n -- R:n )
		wordlink 'a>r', 3
a_to_r:		pop	ax
		push	dx
		jmp	ax

		; ( A:n -- A:n+1 )
		wordlink 'a+', 2
a_inc:		inc dx
		ret

		; ( A:n -- A:n-1 )
		wordlink 'a-', 2
a_dec:		dec dx
		ret

		; ( A:a -- c A:a )
		wordlink 'ac@', 3
a_cfetch:	mov	bx, dx
		mov	bl, [bx]
		xor	bh, bh
		spush	bx
		ret

		; ( c A:a -- A:a )
		wordlink 'ac!', 3
a_cstore:	spop	ax
		mov	bx, dx
		mov	[bx], al
		ret

		; ( A:a -- c A:a+1 )
		wordlink 'ac@+', 4
a_cfetchplus:	mov	bx, dx
		inc	dx
		mov	bl, [bx]
		xor	bh, bh
		spush	bx
		ret

		; ( c A:a -- A:a+1 )
		wordlink 'ac!+', 4
a_cstoreplus:	spop	ax
		mov	bx, dx
		inc	dx
		mov	[bx], al
		ret


; ARITHMETIC / BITS

		; ( a b -- a+b )
		wordlink '+', 1
plus:		spop	bx
		mov	ax, [bp]
		add	bx, ax
		mov	[bp], bx
		ret

		; ( a b -- a-b )
		wordlink '-', 1
minus:		spop	bx
		mov	ax, [bp]
		sub	ax, bx
		mov	[bp], ax
		ret

		; ( a b -- b-a )
		wordlink '-^', 2
minus_opp:	spop	bx
		mov	ax, [bp]
		sub	bx, ax
		mov	[bp], bx
		ret

		; ( a b -- a*b )
		wordlink '*', 1
mul:		spop	bx
		mov	ax, [bp]
		imul	ax, bx
		mov	[bp], ax
		ret

		; ( a b -- a/b )
		wordlink '/', 1
div:		spop	bx
		mov	ax, [bp]
		xor	dx, dx
		div	bx
		mov	[bp], ax
		ret

		; ( n1 n2 -- lo hi )
		wordlink '<>', 2
sort:		spop	bx
		mov	ax, [bp]
		cmp	ax, bx
		jb	.skip
		xchg	ax, bx
		mov	[bp], ax
	.skip	spush	bx
		ret

		; ( n -- n*2 )
		wordlink '2*', 2
shiftl:		mov	ax, [bp]
		shl	ax, 1
		mov	[bp], ax
		ret

		; ( n -- n/2 )
		wordlink '2/', 2
shiftr:		mov	ax, [bp]
		shr	ax, 1
		mov	[bp], ax
		ret

		; ( n -- n<<8 )
		wordlink 'l>m', 3
shiftl8:	mov	ax, [bp]
		shl	ax, 8
		mov	[bp], ax
		ret

		; ( n -- n>>8 )
		wordlink 'm>l', 3
shiftr8:	mov	ax, [bp]
		shr	ax, 8
		mov	[bp], ax
		ret

		; ( n -- msb lsb )
		wordlink 'm|l', 3
bytesplit:	spop	ax
		xor	bx, bx
		mov	bl, ah
		spush	bx
		mov	bl, al
		spush	bx
		ret

		; ( n -- n+1 )
		wordlink '1+', 2
oneplus:	mov	ax, [bp]
		inc	ax
		mov	[bp], ax
		ret

		; ( n -- n-1 )
		wordlink '1-', 2
oneminus:	mov	ax, [bp]
		dec	ax
		mov	[bp], ax
		ret

		; ( n -- n+2 )
		wordlink '2+', 2
twoplus:	mov	ax, [bp]
		add	ax, 2
		mov	[bp], ax
		ret

		; ( n -- n-2 )
		wordlink '2-', 2
twominus:	mov	ax, [bp]
		sub	ax, 2
		mov	[bp], ax
		ret

		; ( n1 n2 -- hi )
		wordlink 'max', 3
max:		spop	bx
		mov	ax, [bp]
		cmp	bx, ax
		jb	.done
		mov	[bp], bx
	.done	ret

		; ( n1 n2 -- lo )
		wordlink 'min', 3
min:		spop	bx
		mov	ax, [bp]
		cmp	ax, bx
		jb	.done
		mov	[bp], bx
	.done	ret

		; ( a b -- a%b )
		wordlink 'mod', 3
mod:		spop	bx
		mov	ax, [bp]
		xor	dx, dx
		div	bx
		mov	[bp], dx
		ret

		; ( a b -- r q )
		wordlink '/mod', 4
divmod:		spop	bx
		mov	ax, [bp]
		xor	dx, dx
		div	bx
		mov	[bp], dx
		spush	ax
		ret

		; ( a b -- a&b )
		wordlink 'and', 3
and:		spop	bx
		mov	ax, [bp]
		and	ax, bx
		mov	[bp], ax
		ret

		; ( a b -- a|b )
		wordlink 'or', 2
or:		spop	bx
		mov	ax, [bp]
		or	ax, bx
		mov	[bp], ax
		ret

		; ( a b -- a^b )
		wordlink 'xor', 3
xor:		spop	bx
		mov	ax, [bp]
		xor	ax, bx
		mov	[bp], ax
		ret

		; ( n u -- n<<u )
		wordlink 'lshift', 6
lshift:		spop	cx
		mov	ax, [bp]
		shl	ax, cl
		mov	[bp], ax
		ret

		; ( n u -- n>>u )
		wordlink 'rshift', 6
rshift:		spop	cx
		mov	ax, [bp]
		shr	ax, cl
		mov	[bp], ax
		ret


; LOGIC

		; = ( n1 n2 -- f )
		wordlink '=', 1
equ:		spop	bx
		mov	ax, [bp]
		mov	word [bp], 0
		cmp	ax, bx
		jne	.done
		not	word [bp]
	.done:	ret

		; < ( n1 n2 -- f )
		wordlink '<', 1
lt:		spop	bx
		mov	ax, [bp]
		mov	word [bp], 0
		cmp	ax, bx
		jge	.done
		not	word [bp]
	.done:	ret

		; > ( n1 n2 -- f )
		wordlink '>', 1
gt:		spop	bx
		mov	ax, [bp]
		mov	word [bp], 0
		cmp	ax, bx
		jle	.done
		not	word [bp]
	.done:	ret

		; <= ( n1 n2 -- f )
		wordlink '<=', 2
lte:		spop	bx
		mov	ax, [bp]
		mov	word [bp], 0
		cmp	ax, bx
		jg	.done
		not	word [bp]
	.done:	ret

		; >= ( n1 n2 -- f )
		wordlink '>=', 2
gte:		spop	bx
		mov	ax, [bp]
		mov	word [bp], 0
		cmp	ax, bx
		jl	.done
		not	word [bp]
	.done:	ret

		; 0< ( n -- f )
		wordlink '0<', 2
zlt:		mov	ax, [bp]
		mov	word [bp], 0
		test	ax, ax
		jge	.done
		not	word [bp]
	.done:	ret

		; 0>= ( n -- f )
		wordlink '0>=', 3
zge:		mov	ax, [bp]
		mov	word [bp], 0
		test	ax, ax
		jl	.done
		not	word [bp]
	.done:	ret

		; not ( f -- f )
		wordlink 'not', 3
not:		not	word [bp]
		ret


; STRINGS

		; compile-time	( xxx" -- )
		wordlink 's"', 2 | FLAG_IMMEDIATE
squote: mov	di, [here]
		litb	0xe9		; near jmp
		mov	ax, di		; rel16
		add	di, 2
		spush	di		; sa
		spush	ax		; rel16
		xor	cx, cx
	.next:	call	tochar
		spop	ax
		xor	ah, ah
		cmp	al, 34
		je	.done
		stosb
		inc	cx
		jmp	.next
	.done:	spop	bx
		mov	ax, di
		sub	ax, bx
		sub	ax, 2
		mov	[bx], ax	; fill rel16 above
		mov	[here], di	; store as literals
		call	litn
		spush	cx
		call	litn
		ret


; NUMBER FORMATTING

		; . ( n -- )
		; print n in its unsigned decimal form
		wordlink 'u.', 2
udot:		spop	ax
	digit:	xor	dx, dx
		mov	bx, 10
		div	bx
		test	ax, ax
		jz	.zero
		push	dx
		call	digit
		pop	dx
	.zero:	push	ax
		mov	al, dl
		add	al, '0'
		spush	ax
		call	emit
		pop	ax
		ret

		; ( n -- )
		; print n in its signed decimal form
		wordlink '.', 1
dot:		spop	ax
		test	ax, ax	; sign?
		jns	digit
		neg	ax
		push	ax
		spush	'-'
		call	emit
		pop	ax
		jmp	digit

		; ( n -- )
		; print n in hex form as a nibble
doth:		spop	ax
		and	al, 0x0f ; mask for the first byte
		cmp	al, 10
		jb	.digit
		add	al, 'a'-'0'-10
	.digit:	add	al, '0'
		mov	ah, 0x0e
		int	0x10
		ret

		; ( n -- )
		; print n in hex form as a byte
		wordlink '.x', 2
dotx:		call	dup
		spush	4
		call	rshift
		call	doth
		call	doth
		ret

		; ( n -- )
		; print n in hex form as a word
		wordlink '.X', 2
dotX:		call	dup
		spush	8
		call	rshift
		call	dotx
		call	dotx
		ret

; I/O

		; ( ..." -- )
		; write ... to here
		wordlink ',"', 2 | FLAG_IMMEDIATE
comma_quote:	mov	di, [here]
	.next:	call	tochar
		spop	ax
		cmp	ax, '"'
		je	.done
		stosb
		jmp	.next
	.done:	mov	[here], di
		ret

		; ( ..." -- )
		; print ... during runtime
		wordlink '."', 2 | FLAG_IMMEDIATE
dot_quote:	call	squote		; ( -- sa sl )
		spush	type
		call	compile_comma
		ret

		; ( a -- )
		; emit line at addr a
		wordlink 'emitln', 6
emitln:		spush	64
		call	type
		ret

		; ( -- c? f )
		; polls the keyboard for a key
		wordlink 'key?', 4
key?:		mov	ah, 0x01
		int	0x16
		jz	.nokey
		mov	ah, 0x00 ; remove key from input buffer
		int	0x16
		spush	ax
		spush	-1
		ret
	.nokey	spush	0
		ret

; BIOS INTERRUPTh

; BROKEN
		; int <num> ( -- ) compile-only
		wordlink 'int', 3 | FLAG_IMMEDIATE
int:		call	toword		; ( -- sa sl )
		call	parse		; ( sa sl -- n? f )
		spop	ax
		test	ax, ax
		jz	notnumber
		mov	di, [here]
		litb	0xcd ; int opcode
		spop	ax
		stosb
		mov	[here], di
		ret

		; int10h
		wordlink 'int10h', 6
int10h:		int	10h
		ret

		; ( ax -- )
		wordlink '>ax', 3
to_ax:		spop	ax
		ret

		; ( bx -- )
		wordlink '>bx', 3
to_bx:		spop	bx
		ret

		; ( cx -- )
		wordlink '>cx', 3
to_cx:		spop	cx
		ret

		; ( dx -- )
		wordlink '>dx', 3
to_dx:		spop	dx
		ret

		; ( -- ax )
		wordlink 'ax>', 3
ax_from:	spush	ax
		ret

		; ( -- bx )
		wordlink 'bx>', 3
bx_from:	spush	bx
		ret

		; ( -- cx )
		wordlink 'cx>', 3
cx_from:	spush	cx
		ret

		; ( -- dx )
		wordlink 'dx>', 3
dx_from:	spush	dx
		ret

		; ( -- f )
		wordlink 'flags>', 6
f_from:		pushf
		pop	ax
		spush	ax
		ret

; | Bit | Mask  | Meaning               |
; |-----|-------|-----------------------|
; |   0 | 0001h | CF – Carry            |
; |   2 | 0004h | PF – Parity           |
; |   4 | 0010h | AF – Aux carry        |
; |   6 | 0040h | ZF – Zero             |
; |   7 | 0080h | SF – Sign             |
; |   9 | 0200h | IF – Interrupt Enable |
; |  10 | 0400h | DF – Direction        |
; |  11 | 0800h | OF – Overflow         |


; OTHER

		; ( -- h l )
		; return current system clock counter
		wordlink 'ticks', 5
ticks:		xor	ah, ah
		int	0x1a
		spush	cx	; higher order 
		spush	dx	; lower order 
		ret		; higher order at cx

		; ( h l -- )
		; wait in microseconds (double number)
		wordlink 'usleep', 6
usleep:		mov	ah, 0x86
		spop	dx	; lower order
		spop	cx	; higher order
		int	0x15	; cx:dx wait
		ret

		wordlink 'reboot', 6
boot:		int	19h

		wordlink 'noop', 4
noop:		ret

end:
