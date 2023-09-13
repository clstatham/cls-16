%start
  sub fp fp $0x4
  mov sp fp
  mov fp $0xf000
  mov sp fp
%startbody
%startbinary0
    mov r5 $0x2a
    stl fp r5 $0x4
    sth fp r5 $0x5
  add r5 fp $0x4
%startbinary1
    stl fp r5 $0x2
    sth fp r5 $0x3
  ldl r4 fp $0x2
  ldh r4 fp $0x3
  ldl r2 r4 $0x0
  ldh r2 r4 $0x1
  printi r2
%startbinary2
    ldl r2 fp $0x2
    ldh r2 fp $0x3
    mov r6 $0x2b
    stl r2 r6 $0x0
    sth r2 r6 $0x1
  ldl r2 fp $0x4
  ldh r2 fp $0x5
  printi r2
  add fp fp $0x4
  mov sp fp
%startreturn
  halt 