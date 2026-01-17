: .h $f and dup 10 < if '0' else $37 then + emit ; \ nibble
: .x dup 4 rshift .h .h ; \ byte
: .X dup 8 rshift .x .x ; \ word

: ' word find drop ; : ['] ' ; immediate
: [compile] ' compile, ; immediate
: postpone ' litn [ ' compile, litn ] compile, ; immediate
: ." [compile] s" postpone stype ; immediate

: .ki ( n -- ) 10 rshift . ; \ kibi
: memused here @ $7e00 - ; : memfree $7fff memused - ;
: .memstat memused .ki ." KiB used " memfree .ki ." KiB free" ;

here @ ] s" Welcome to TANUKI OS" stype ; execute
nl> .memstat

quit
