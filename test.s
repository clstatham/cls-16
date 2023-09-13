%start
  sub fp fp $0x2
  mov sp fp
  mov fp $0xf000
  mov sp fp
%startbody
  %startbinary0
    mov r5 $0x64
    stl fp r5 $0x2
    sth fp r5 $0x3
%startcond1
  %startbinary1
    ldl r4 fp $0x2
    ldh r4 fp $0x3
    mov r6 $0xa
    sub r0 r4 r6
    jz %startbinary1false0
    jc %startbinary1false0
    jmp %startbinary1true0
  %startbinary1true0
    mov r2 $0x1
    jmp %startbinary1end0
  %startbinary1false0
    mov r2 $0x0
  %startbinary1end0
  mov r5 r2
  sub r0 r5 r0
  jz %startend1
  jmp %startbody1
%startbody1
  %startblock2
    %startblock2binary0
      ldl r2 fp $0x2
      ldh r2 fp $0x3
      div r2 r2 $0x2
      stl fp r2 $0x2
      sth fp r2 $0x3
    ldl r2 fp $0x2
    ldh r2 fp $0x3
    printi r2
  jmp %startcond1
%startend1
  add fp fp $0x2
  mov sp fp
%startreturn
  halt 