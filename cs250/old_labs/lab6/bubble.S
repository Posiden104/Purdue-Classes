.section        .data
.section        .text
.global         bubble
bubble:
        nop                 @no operation
        mov r7, $1          @exit syscall
        svc $0              @wake kernel
        .end