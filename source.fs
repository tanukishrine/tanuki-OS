\ KERNEL WORDS
\ ( a R: b -- R: a b ) safe push to return stack for NEXT loops
: >r? r> r> rot >r >r >r ;  ( a R: b -- R: a b )
\ ( R: a b -- a R: b ) likewise, pop from return stack
: r>? r> r> r> nrot >r >r ; ( R: a b -- a R: b )
\ ( a n -- ) hexdump n lines from addr a
: dump ( a n -- ) >r begin nl> ':' emit dup .x spc>
  4 >r begin dup c@ dup .x >r? 1+ dup c@ dup .x >r? 1+ spc> next
  8 >r begin r>? next 8 >r begin dup $20 < if drop '.' then emit
  next next drop ;
\ ( -- ) display all visible words in the dictionary
: words latest @ begin 3 - dup over 2+ c@ $7f and
  dup >r - r> type spc> @ dup 0 = until ;



\ BLOCK WORDS
here @ 1024 allot value blk(  \ start addr of blk buf
blk( 1024 + value blk)        \ end addr of blk buf
-1 value blk>                 \ currently selected blk
: >blk ( n -- addr ) 10 lshift $7e00 + ; \ find addr of blk n
: load ( n -- ) >blk blk ! 16 line ! ; \ interpret blk n
\ ( n1 n2 -- ) load blocks between n1 and n2, inclusive
: thru over - 1+ 4 lshift line ! >blk blk ! ;
\ ( n -- ) display n right aligned by 2 spaces
: .2 nl> dup 10 < if spc> then . spc> ;
\ ( addr -- ) print the contents as a block at addr
: display 16 >r begin 16 r@ - .2 dup emitln 64 + next drop ;
: list ( n -- ) >blk display ; \ print contents of blk n
: index 16 >r begin 16 r@ - .2 dup >blk emitln 1+ next drop ;
\ copy contents of block s to block d
: copy ( s d -- ) >blk >r >blk r> 1024 move ;
\ BOOT MESSAGE
: memused here @ $500 - ; : memfree $7bff memused - ;
: memstat \ prints current dictionary usage
  memused 1000 / . ." kB used " memfree 1000 / . ." kB free" ;
here @ ," Welcome to TANUKI OS" 20 type nl> memstat

3 10 thru








\ CURSOR CONTROL AND COLORED EMIT
\ ( row col -- ) set cursor position
: >cursor swap l>m xor >dx $0200 >ax 0 >bx int10h ;
\ ( --- row col ) fetch cursor position
: cursor> $0300 >ax 0 >bx int10h dx> m|l ;
\ ( row col -- row col )
: norm-col dup 79 > if 80 - swap 1+ swap then ;
\ ( rgb char -- ) write char with color
: cwrite $0900 xor >ax >bx 1 >cx int10h ;
\ ( rgb char -- ) like cwrite, but progress cursor
: cemit cwrite cursor> 1+ norm-col >cursor ;
\ ( -- ) clear the page and reset cursor
: page $0600 >ax $0700 >bx 0 >cx $184f >dx int10h 0 0 >cursor ;
\ ( rgb addr len -- ) like type, but with attributes
: ctype >r begin 2dup c@ cemit 1+ next ; 

\ BIOS COLOR ATTRIBUTES
: black   $0 ; : white         $f ;
: blue    $1 ; : yellow        $e ;
: green	  $2 ; : lightmagenta  $d ;
: cyan	  $3 ; : lightred      $c ;
: red	  $4 ; : lightcyan     $b ;
: magenta $5 ; : lightgreen    $a ;
: brown   $6 ; : lightblue     $9 ;
: gray    $7 ; : lightgray     $8 ;

\ ( bg fg -- ) combine background and foreground color
: >bf swap 4 lshift xor ;

: hello brown yellow >bf s" This is a very long string!" ctype ;


\ GAME
80 25 * value SCREEN_SIZE
0 value screen here @ to screen SCREEN_SIZE allot0
screen SCREEN_SIZE 250 fill
: reset-cursor 0 0 >cursor ;
\ print content of screen
: blip reset-cursor brown yellow >bf screen SCREEN_SIZE ctype ;
0 value gamegrid here @ to gamegrid SCREEN_SIZE allot0

\ KEY DEBUG
: ?exit ( c -- ) $2e03 = if rdrop then ; \ exit word if ctrl-c
: keydump ( -- ) begin key? if dup nl> .X ?exit then again ;
: ?abort ( -- ) key? if $2e03 = if abort" aborted!" then then ;
: test 0 begin dup . spc> 1+ ?abort again ;


40 value Px
12 value Py
: Px- Px 1- to Px ;
: Py- Py 1- to Py ;
: Py+ Py 1+ to Py ;
: Px+ Px 1+ to Px ;
: .pos nl> Px . spc> Py . ;
0 value key>
: run begin key? if
    dup ?exit $00ff and to key>
    key> 'h' = if Px- then
    key> 'j' = if Py- then
    key> 'k' = if Py+ then
    key> 'l' = if Px+ then
    .pos
  then again ;
