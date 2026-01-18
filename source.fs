: [compile] ' compile, ; immediate
: postpone ' litn [ ' compile, litn ] compile, ; immediate
: >r? r> r> rot >r >r >r ;  ( a R: b -- R: a b )
: r>? r> r> r> nrot >r >r ; ( R: a b -- a R: b )
: dump ( n a -- ) $fff0 and swap >r begin nl> dup .X ." : "
  8 >r begin dup @ .X spc> dup c@ >r? 1+ dup c@ >r? 1+ next
  16 >r begin r>? next ." | " 16 >r begin dup $20 < if drop '.'
  then emit next next drop ;
: memused here @ $500 - ; : memfree $7bff memused - ;
: memstat
  memused 1000 / . ." kB used " memfree 1000 / . ." kB free" ;

here @ ," Welcome to TANUKI OS" 20 type nl> memstat

quit
