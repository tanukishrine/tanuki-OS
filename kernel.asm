BITS 16
org 0x7e00

; bp = PSP (parameter stack pointer) ; sp = RSP (return stack pointer) ; FORTH MACROS
%macro	pspush	1	; src
	sub	bp, 2
	mov	word [bp], %1
%endmacro
%macro	pspop	1	; dst
	mov	word %1, [bp]
	add	bp, 2
%endmacro

; WORD STRUCTURE

%macro wordinit 2
	db	%1	; name (n bytes)
	dw	0	; link (2 bytes)
	db	%2	; len  (1 byte)
	%push dict
	%$link:
%endmacro		; load (n bytes)

%macro wordlink 2
	db	%1	; name (n bytes)
	dw	%$link	; link (2 bytes)
	db	%2	; len  (1 byte)
	%pop
	%push dict
	%$link:
%endmacro		; load (n bytes)

%macro wordlast 2
	db	%1	; name (n bytes)
	dw	%$link	; link (2 bytes)
	db	%2	; len  (1 byte)
%endmacro		; load (n bytes)

%define	FLAG_IMMEDIATE	0x80	; immediate flag
%define	MASK_LENGTH	0x7f	; namelen mask

start:		mov	bp, 0xff00	; initialize SP
		mov	sp, 0x0000	; initialize RS
		jmp	interpret

; SYSTEM VARIABLES

state:		db 0

latest:		dw nop
here:		dw end

in:		dw buffer
buffer:		times 65 db 0

curchar:	dw buffer
curlen:		db 0

blk:		dw 0x0500	; start of source


; DICTIONARY

		; key ( -- c )
		; fetch char c from direct input

		wordinit 'key', 3

key:		mov	ah, 0x00
		int	0x16
		xor	ah, ah
		pspush	ax
		ret


		; emit ( c -- )
		; spit char c to output stream

		wordlink 'emit', 4

emit:		pspop	ax
		mov	ah, 0x0e
		int	0x10
		ret


		; abort ( -- )
		; reset PS and jump to quit

		wordlink 'abort', 5

abort:		mov	bp, 0xff00
		jmp	quit


		; quit ( -- )
		; reset RS, clear the buffer,
		; set state to immediate mode,
		; jump to interpret loop

		wordlink 'quit', 4

quit:		mov	sp, 0x0000
		mov	[state], 0	; interpret mode
		mov	word [blk], 0	; input from user
		mov	[in], buffer+64	; initialize rdln
		jmp	interpret


		; interpret ( -- )
		; main interpret loop
		wordlink 'interpret', 9

interpret:	; read next word
		call	toword		; ( -- sa sl )

		; is a number?
		call	parse		; ( sa sl -- n? f )
		pspop	ax		; ( n? f -- n? )
		test	ax, ax
		jnz	.num

		; is a word?
		call	curword		; ( -- sa sl )
		call	find		; ( sa sl -- w? f )
		pspop	ax		; ( w? f -- w? )
		test	ax, ax
		jnz	.word

		; not a word
		call	nl_out		; ( -- )
		call	curword		; ( -- sa sl )
		call	stype		; ( sa sl -- )
		pspush	.err		; ( -- sa )
		pspush	15		; ( -- sa sl )
		call	stype		; ( sa sl -- )
		jmp	abort

	.err:	db ' word not found'

	.num:	mov	al, [state]
		test	al, al
		jz	interpret

		call	litn
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

		call	compile_comma
		jmp	interpret

	.run:	call	execute
		jmp	interpret


		; stype ( sa sl -- )
		; emit all chars of string

		wordlink 'stype', 5
stype:		pspop	cx
		pspop	si
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
		pspop	ax
		cmp	ax, 0x20
		jbe	toword
		mov	ax, [in]
		dec	ax
		pspush	ax
		mov	[curchar], ax
		xor	cx, cx

	.next:	inc	cx
		call	tochar
		pspop	ax
		cmp	ax, 0x20
		ja	.next

		pspush	cx
		mov	[curlen], cl
		ret

		; curword ( -- sa sl )
		; yield the last read word

		wordlink 'curword', 7

curword:	mov	ax, [curchar]
		pspush	ax
		mov	al, [curlen]
		xor	ah, ah
		pspush	ax
		ret


		; in< ( -- c )
		; read one char from buffered input
		; if end of input, read new line

		wordlink 'in<', 3

tochar:		mov	bx, [in]
		cmp	bx, buffer+64
		jbe	.skip
		call	ln_in
		mov	bx, buffer
	.skip:	mov	al, [bx]
		xor	ah, ah
		pspush	ax
		inc	bx
		mov	[in], bx
		ret


		; ln< ( -- )
		; routine that feeds lines to the interpreter
		wordlink 'ln<', 3
ln_in:		mov	ax, [blk]
		test	ax, ax
		jz	rdln
		jmp	rdbln

		; read block line
		wordlink 'rdbln', 5
rdbln:		mov	si, [blk]
		mov	di, buffer
		mov	cx, 64
		rep	movsb
		mov	al, 0
		stosb
		mov	word [blk], si
		mov	[in], buffer
		ret

		; rdln ( -- )
		; feed a line to the buffered input

		wordlink 'rdln', 4

rdln:		pspush	.ok
		pspush	5
		call	stype

		mov	di, buffer

	.next	cmp	di, buffer+64	; buffer full?
		je	.cr		; submit

		call	key
		pspop	ax

		cmp	al, 0x0d
		je	.cr

		cmp	al, 0x08
		je	.bs

		cmp	al, 0x20	; control character?
		jb	.next		; do nothing

		stosb			; store string byte
		pspush	ax
		call	emit
		jmp	.next

	.bs:	cmp	di, buffer	; empty buffer?
		je	.next		; do nothing

		dec	di		; clear previous character
		mov	byte [di], 0
		call	bs_out
		jmp	.next

	.cr:	call	spc_out		; QOL visual
		xor	al, al		; flush rest of line with null
	.null:	stosb			
		cmp	di, buffer+64
		jbe	.null
		mov	[in], buffer	; reset input ptr
		ret

	.ok	db ' ok', 13, 10


		; parse ( sa sl -- n? f )
		; convert string as a number

		wordlink 'parse', 5

parse:		pspop	cx
		pspop	si
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

	.digit:	sub	al, '0'
		jmp	.add

	.alpha:	sub	al, 'a'-10

	.add:	add	bx, ax
		loop	.hex

		jmp	.num

		; negative decimal
	.neg:	lodsb
		mul	bx, 10
		sub	al, '0'
		jb	.nan
		cmp	al, 10
		jae	.nan
		sub	bx, ax
		loop	.neg
		jmp	.num

		; unsigned decimal
	.pos:	lodsb
		mul	bx, 10
	.skip:	sub	al, '0'
		jb	.nan
		cmp	al, 10
		jae	.nan
		add	bx, ax
		loop	.pos

	.num:	pspush	bx
		pspush	-1
		ret

	.nan:	pspush	0
		ret


		; lit ( -- n ) runtime

lit:		pop	si
		lodsw
		pspush	ax
		push	si
		ret


		; litn ( n -- )
		; write n as a literal

		wordlink 'litn', 4

litn:		mov	al, 0xe8	; call opcode
		mov	di, [here]
		stosb

		mov	ax, lit		; relative addressing
		sub	ax, di
		sub	ax, 2
		stosw

		pspop	ax		; store n
		stosw

		mov	[here], di
		ret


		; find ( sa sl -- w? f )
		wordlink 'find', 4
find:		pspop	ax		; len
		pspop	dx		; addr
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

		pspush	0
		ret

	.found:	pspush	bx
		pspush	-1
		ret


		; execute ( w -- )
		; jump IP to addr w

		wordlink 'execute', 7

execute:	pspop	ax
		jmp	ax


		; compile, ( w -- )
		; append a call to wordref w to here

		wordlink 'compile,', 8

compile_comma:	mov	al, 0xe8 ; call opcode
		mov	di, [here]
		stosb

		pspop	ax ; wordref
		sub	ax, di
		sub	ax, 2
		stosw

		mov	[here], di
		ret


		; header ( sa sl -- )
		; append a new header with name s

		wordlink 'header', 6

header:		pspop	bx
		mov	cx, bx
		pspop	si
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

		wordlink 'exit', 4

exit:		mov	al, 0xc3 ; ret
		mov	di, [here]
		stosb
		mov	[here], di
		ret

		; : x ( -- )
		; create a new word definition with name x

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


		; spc> ( -- ) emit space character

		wordlink 'spc>', 4

spc_out:	pspush	0x20 ; SP
		call	emit
		ret


		; nl> ( -- ) emit newline
	
		wordlink 'nl>', 3

nl_out:		pspush	.nl
		pspush	2
		call	stype
		ret

		; CR, LF
	.nl:	db 0x0d, 0x0a


		; bs> ( -- )
		; delete prev char in TTY mode

		wordlink 'bs>', 3

bs_out:		pspush	.bs
		pspush	3
		call	stype
		ret

		; BS, SPC, BS
	.bs:	db 0x08, 0x20, 0x08


; FLOW

		; ( -- )
		; ignore input until ')' is read
		wordlink '(', 1 | FLAG_IMMEDIATE
paren:		call	tochar
		pspop	ax
		cmp	ax, ')'
		jne	paren
		ret

		; ( -- )
		; ignore input until end of line
		wordlink '\', 1 | FLAG_IMMEDIATE
backslash:	mov	[in], buffer+64
		ret

%macro	stolit	1
	mov	byte [di], %1
	inc	di
%endmacro

; 		; compile-time	( -- a )
; 		; run-time	( f -- )
; 		wordlink 'if', 2 | FLAG_IMMEDIATE
; if:		mov	di, [here]
; 		stolit	0x8b		; mov ax, [bp]
; 		stolit	0x46
; 		stolit	0x00
; 		stolit	0x83		; add bp, 2
; 		stolit	0xc5
; 		stolit	0x02
; 		stolit	0x85		; test ax, ax
; 		stolit	0xc0
; 		stolit	0x74		; jz rel8
; 		pspush	di
; 		inc	di
; 		mov	[here], di
; 		ret
; 
; 		; compile-time	( a1 -- a2 )
; 		; run-time	( -- )
; 		wordlink 'else', 4 | FLAG_IMMEDIATE
; else:		mov	di, [here]	; compile
; 		stolit	0xeb		; jmp rel8
; 		pspush	di
; 		inc	di
; 		mov	[here], di
; 		call	swap
; 		jmp	then		; compile rel8 to IF
; 
; 		; compile-time	( a -- )
; 		; run-time	( -- )
; 		wordlink 'then', 4 | FLAG_IMMEDIATE
; then:		pspop	bx
; 		mov	ax, [here]
; 		sub	ax, bx
; 		dec	ax
; 		mov	[bx], al
; 		ret
; 
; 		; compile-time	( -- a )
; 		; run-time	( -- )
; 		wordlink 'begin', 5 | FLAG_IMMEDIATE
; begin:		mov	ax, [here]
; 		pspush	ax
; 		ret
; 
; 		wordlink 'until', 5 | FLAG_IMMEDIATE
; until:		mov	di, [here]
; 		stolit	0x8b		; mov ax, [bp]
; 		stolit	0x46
; 		stolit	0x00
; 		stolit	0x83		; add bp, 2
; 		stolit	0xc5
; 		stolit	0x02
; 		stolit	0x85		; test ax, ax
; 		stolit	0xc0
; 		stolit	0x74		; (short) jz xxx ; 		pspop	ax ; 		sub	ax, di
; 		dec	ax
; 		stosb
; 		mov	[here], di
; 		ret

		; compile-time	( -- a )
		; run-time	( f -- )
		wordlink 'if', 2 | FLAG_IMMEDIATE
if:		mov	di, [here]
		stolit	0x8b		; mov ax, [bp]
		stolit	0x46
		stolit	0x00
		stolit	0x83		; add bp, 2
		stolit	0xc5
		stolit	0x02
		stolit	0x85		; test ax, ax
		stolit	0xc0
		stolit	0x0f		; jz rel16
		stolit	0x84
		pspush	di		; leave 2 byte space
		add	di, 2
		mov	[here], di
		ret

		; compile-time	( a1 -- a2 )
		; run-time	( -- )
		wordlink 'else', 4 | FLAG_IMMEDIATE
else:		mov	di, [here]	; compile
		stolit	0xe9		; jmp rel8
		pspush	di
		add	di, 2		; leave 2 byte space
		mov	[here], di
		call	swap
		jmp	then		; compile rel16 to 'if'

		; compile-time	( a -- )
		; run-time	( -- )
		wordlink 'then', 4 | FLAG_IMMEDIATE
then:		pspop	bx
		mov	ax, [here]
		sub	ax, bx
		sub	ax, 2
		mov	[bx], ax
		ret

		; compile-time	( -- a )
		; run-time	( -- )
		wordlink 'begin', 5 | FLAG_IMMEDIATE
begin:		mov	ax, [here]
		pspush	ax
		ret

		wordlink 'until', 5 | FLAG_IMMEDIATE
until:		mov	di, [here]
		stolit	0x8b		; mov ax, [bp]
		stolit	0x46
		stolit	0x00
		stolit	0x83		; add bp, 2
		stolit	0xc5
		stolit	0x02
		stolit	0x85		; test ax, ax
		stolit	0xc0
		stolit	0x0f		; jz xxx
		stolit	0x84
		pspop	ax
		sub	ax, di
		sub	ax, 2
		stosw
		mov	[here], di
		ret

		; works outside of definitions
		; [then] cannot be nested
		; all [if] leads to the first [then]
		wordlink '[if]', 4 | FLAG_IMMEDIATE
meta_if:	pspop	ax
		test	ax, ax
		jz	.jump
		ret
	.jump:	call	toword
		pspop	ax
		cmp	ax, 6
		je	.next
		add	bp, 2 ; drop
		jmp	.jump
	.next:	pspush	.then
		pspush	ax
		call	scmp
		pspop	ax
		test	ax, ax
		jz	.jump
		ret
	.then:	db '[then]'

		wordlink '[then]', 6
meta_then:	ret


; SYSTEM VARIABLES

		; ( -- a )
		wordlink 'here', 4
here_addr:	pspush	here
		ret

		; ( -- a )
		wordlink 'latest', 6
latest_addr:	pspush	latest
		ret

		; ( -- a )
blk_addr:	pspush	blk
		ret


; PARAMETER STACK

		; ( a -- )
		wordlink 'drop', 4
drop:		add	bp, 2
		ret

		; ( a -- a a )
		wordlink 'dup', 3
dup:		mov	ax, [bp]
		pspush	ax
		ret

		; ( a -- a a? )
		; dup if a is nonzero
		wordlink '?dup', 4
?dup:		mov	ax, [bp]
		test	ax, ax
		jz	.zero
		pspush	ax
	.zero:	ret

		; ( a b -- b )
		wordlink 'nip', 3
nip:		pspop	ax
		mov	[bp], ax
		ret

		; ( a b -- a b a )
		wordlink 'over', 4
over:		mov	bx, bp
		add	bx, 2
		mov	bx, [bx]
		pspush	bx
		ret

		; ( a b c -- b c a )
		wordlink 'rot', 3
rot:		pspop	ax
		pspop	bx
		mov	cx, [bp]
		mov	[bp], bx
		pspush	ax
		pspush	cx
		ret

		; ( a b c -- c a b )
		wordlink 'nrot', 4
nrot:		pspop	ax
		pspop	bx
		mov	cx, [bp]
		mov	[bp], ax
		pspush	cx
		pspush	bx
		ret

		; ( a b -- b a )
		wordlink 'swap', 4
swap:		pspop	ax
		mov	bx, [bp]
		mov	[bp], ax
		pspush	bx
		ret

		; ( a b -- b a b )
		wordlink 'tuck', 4
tuck:		pspop	ax
		mov	bx, [bp]
		mov	[bp], ax
		pspush	bx
		pspush	ax
		ret

		; ( a a -- )
		wordlink '2drop', 5
twodrop:	add	bp, 4
		ret

		; ( a b -- a b a b )
		wordlink '2dup', 4
twodup:		pspop	ax
		mov	bx, [bp]
		sub	bp, 4
		mov	[bp], bx
		pspush	ax
		ret


; RETURN STACK

		; ( n -- R:n )
		wordlink '>r', 2
to_r:		pspop	ax
		pop	bx
		push	ax
		push	bx
		ret

		; ( R:n -- n )
		wordlink 'r>', 2
r_from:		pop	ax
		pop	bx
		push	ax
		pspush	bx
		ret

		; ( R:n -- n R:n )
		wordlink 'r@', 2
r_fetch:	pop	ax
		pop	bx
		pspush	bx
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
		pspop	cx
		shr	cx, 1
		mov	si, 0xfefe
		std

	.next:	test	cx, cx
		jz	.done
		dec	cx
		lodsw
		pspush	ax
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
		pspush	ax
		ret


		; rcnt ( -- n )
		; size of RS in bytes

		wordlink 'rcnt', 4

rcnt:		mov	ax, 0x0000
		sub	ax, sp
		pspush	ax
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
store:		pspop	bx
		pspop	ax
		mov	[bx], ax
		ret

		; ( n -- )
		; write n in here and advance it
		wordlink ',', 1
comma:		pspop	ax
		mov	di, [here]
		stosw
		mov	[here], di
		ret

		; ( n a -- )
		; increase value at addr a by n
		wordlink '+!', 2
plus_store:	pspop	bx
		pspop	ax
		add	[bx], ax
		ret

		; ( a1 a2 u -- f )
		; compare u bytes between a1 and a2
		wordlink '[]=', 3
scmp:		pspop	cx
		pspop	si
		pspop	di
		repe	cmpsb
		jz	.true
		pspush	0
		ret
	.true:	pspush	-1
		ret

		; ( c a u -- i )
		; look for c within u bytes at addr a
		wordlink '[c]?', 4
c_find:		pspop	cx
		pspop	si
		pspop	ax
		xor	bx, bx
	.next	cmp	[si], al
		je	.true
		inc	bx
		inc	si
		loop	.next
		pspush	-1
		ret
	.true	pspush	bx
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
		pspush	ax
		ret

		; ( c a -- )
		; store byte c in addr a
		wordlink 'c!', 2
c_store:	pspop	bx
		pspop	ax
		mov	[bx], al
		ret

		; ( c a -- a+1 )
		; store byte c in addr a and inc a
		wordlink 'c!+', 3
c_store_plus:	pspop	bx
		mov	ax, [bp]
		mov	[bx], al
		inc	bx
		mov	[bp], bx
		ret

		; ( c -- )
		; store byte c in here and advance it
		wordlink 'c,', 2
c_comma:	pspop	ax
		mov	bx, [here]
		mov	[bx], al
		inc	bx
		mov	[here], bx
		ret

		; ( n -- )
		; move here by n bytes
		wordlink 'allot', 5
allot:		pspop	ax
		mov	bx, [here]
		add	bx, ax
		mov	[here], bx
		ret

		; ( n -- )
		; allot n bytes and fill with zero
		wordlink 'allot0', 6
allot0:		pspop	cx
		mov	di, [here]
		xor	ax, ax
		rep	stosb
		mov	[here], di
		ret

		; ( a n c -- )
		; fill n bytes at addr a with char c
		wordlink 'fill', 4
fill:		pspop	ax
		pspop	cx
		pspop	di
		rep	stosb
		ret

		; ( a1 a2 u -- )
		; copy u bytes from a1 to a2
		wordlink 'move', 4
move:		pspop	cx
		pspop	di
		pspop	si
		rep	movsb
		ret

		; ( a u -- )
		; copy u bytes from a to here
		wordlink 'move,', 5
move_comma:	pspop	cx
		pspop	si
		mov	di, [here]
		rep	movsb
		ret


; ARITHMETIC / BITS

		; ( a b -- a+b )
		wordlink '+', 1
plus:		pspop	bx
		mov	ax, [bp]
		add	bx, ax
		mov	[bp], bx
		ret

		; ( a b -- a-b )
		wordlink '-', 1
minus:		pspop	bx
		mov	ax, [bp]
		sub	ax, bx
		mov	[bp], ax
		ret

		; ( a b -- b-a )
		wordlink '-^', 2
minus_opp:	pspop	bx
		mov	ax, [bp]
		sub	bx, ax
		mov	[bp], bx
		ret

		; ( a b -- a*b )
		wordlink '*', 1
mul:		pspop	bx
		mov	ax, [bp]
		mul	ax, bx
		mov	[bp], ax
		ret

		; ( a b -- a/b )
		wordlink '/', 1
div:		pspop	bx
		mov	ax, [bp]
		xor	dx, dx
		div	bx
		mov	[bp], ax
		ret

		; ( n1 n2 -- lo hi )
		wordlink '<>', 2
sort:		pspop	bx
		mov	ax, [bp]
		cmp	ax, bx
		jb	.skip
		xchg	ax, bx
		mov	[bp], ax
	.skip	pspush	bx
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
max:		pspop	bx
		mov	ax, [bp]
		cmp	bx, ax
		jb	.done
		mov	[bp], bx
	.done	ret

		; ( n1 n2 -- lo )
		wordlink 'min', 3
min:		pspop	bx
		mov	ax, [bp]
		cmp	ax, bx
		jb	.done
		mov	[bp], bx
	.done	ret

		; ( a b -- a%b )
		wordlink 'mod', 3
mod:		pspop	bx
		mov	ax, [bp]
		xor	dx, dx
		div	bx
		mov	[bp], dx
		ret

		; ( a b -- r q )
		wordlink '/mod', 4
divmod:		pspop	bx
		mov	ax, [bp]
		xor	dx, dx
		div	bx
		mov	[bp], dx
		pspush	ax
		ret

		; ( a b -- a&b )
		wordlink 'and', 3
and:		pspop	bx
		mov	ax, [bp]
		and	ax, bx
		mov	[bp], ax
		ret

		; ( a b -- a|b )
		wordlink 'or', 2
or:		pspop	bx
		mov	ax, [bp]
		or	ax, bx
		mov	[bp], ax
		ret

		; ( a b -- a^b )
		wordlink 'xor', 3
xor:		pspop	bx
		mov	ax, [bp]
		xor	ax, bx
		mov	[bp], ax
		ret

		; ( n u -- n<<u )
		wordlink 'lshift', 6
lshift:		pspop	cx
		mov	ax, [bp]
		shl	ax, cx
		mov	[bp], ax
		ret

		; ( n u -- n>>u )
		wordlink 'rshift', 6
rshift:		pspop	cx
		mov	ax, [bp]
		shr	ax, cx
		mov	[bp], ax
		ret


; LOGIC

		; = ( n1 n2 -- f )
		wordlink '=', 1
equ:		pspop	bx
		mov	ax, [bp]
		mov	word [bp], 0
		cmp	ax, bx
		jne	.done
		not	word [bp]
	.done:	ret

		; < ( n1 n2 -- f )
		wordlink '<', 1
lt:		pspop	bx
		mov	ax, [bp]
		mov	word [bp], 0
		cmp	ax, bx
		jge	.done
		not	word [bp]
	.done:	ret

		; > ( n1 n2 -- f )
		wordlink '>', 1
gt:		pspop	bx
		mov	ax, [bp]
		mov	word [bp], 0
		cmp	ax, bx
		jle	.done
		not	word [bp]
	.done:	ret

		; <= ( n1 n2 -- f )
		wordlink '<=', 2
lte:		pspop	bx
		mov	ax, [bp]
		mov	word [bp], 0
		cmp	ax, bx
		jg	.done
		not	word [bp]
	.done:	ret

		; >= ( n1 n2 -- f )
		wordlink '>=', 2
gte:		pspop	bx
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
squote:	mov	di, [here]
		stolit	0xe9		; near jmp
		mov	ax, di		; rel16
		add	di, 2
		pspush	di		; sa
		pspush	ax		; rel16
		xor	cx, cx
	.next:	call	tochar
		pspop	ax
		xor	ah, ah
		cmp	al, 34
		je	.done
		stosb
		inc	cx
		jmp	.next
	.done:	pspop	bx
		mov	ax, di
		sub	ax, bx
		sub	ax, 2
		mov	[bx], ax	; fill rel16 above
		mov	[here], di	; store as literals
		call	litn
		pspush	cx
		call	litn
		ret

; 		; compile-time	( xxx" -- )
; 		wordlink 's"', 2 | FLAG_IMMEDIATE
; squote:		mov	di, [here]
; 		mov	[di], 0xe9 ; 16bit jmp
; 		inc	di
; 		mov	[.ra], di
; 		add	di, 2
; 		mov	[.sa], di
; 		xor	cx, cx
; 	.next:	call	tochar
; 		pspop	ax
; 		cmp	al, 34 ; '"'
; 		je	.done
; 		stosb
; 		inc	cx
; 		jmp	.next
; 	.done:	mov	ax, di
; 		mov	bx, [.ra]
; 		sub	ax, bx
; 		sub	ax, 2
; 		mov	[bx], ax
; 		mov	[here], di
; 		mov	ax, [.sa]
; 		pspush	ax
; 		call	litn
; 		pspush	cx
; 		call	litn
; 		ret
; 
; 	.ra:	dw	0
; 	.sa:	dw	0
; 	.sl:	dw	0



; NUMBER FORMATTING

		; . ( n -- )
		; print n in its unsigned decimal form
		wordlink 'u.', 2
udot:		pspop	ax
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
		pspush	ax
		call	emit
		pop	ax
		ret

		; ( n -- )
		; print n in its signed decimal form
		wordlink '.', 1
dot:		pspop	ax
		test	ax, ax	; sign?
		jns	digit
		neg	ax
		push	ax
		pspush	'-'
		call	emit
		pop	ax
		jmp	digit

		; ( n -- )
		wordlink '.x', 2
dotx:		pspop	ax
		mov	ah, al
		shr	ah, 4

		cmp	al, 10
		jb	.numb1
		add	al, 'A'
		jmp	.skip1
	.numb1:	add	al, '0'

	.skip1:	cmp	ah, 10
		jb	.numb2
		add	ah, 'A'
		jmp	.skip2
	.numb2:	add	ah, '0'

	.skip2:	xor	bh, bh
		mov	bl, ah
		pspush	bx
		call	emit
		mov	bl, al
		pspush	bx
		call	emit
		ret
		


		wordlast 'nop', 3
nop:		ret


%macro line 1
	%strlen __len %1
	db %1
	times (64 - __len) db 32
%endmacro

end:		db 237
