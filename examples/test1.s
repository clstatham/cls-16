%start
    ldi     sp $0x1000
    printi  sp
    ldi     r1 $0x1337
    printi  r1
    push    r1
    printi  sp
    pop     r2
    printi  sp
    printi  r2
    halt
