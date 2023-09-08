%start
    ; comment
    ldi     r1 $1337
    ldi     r6 %timestwo
    jmp     r6
%printhalt
    printi  r1
    halt
%timestwo
    add     r1 r1 r1
    ldi     r6 %printhalt
    jmp     r6
