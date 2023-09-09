%start
    b
    ; setup initial stack frame
    mov     sp $0xF000
    mov     fp sp

%done
    halt
