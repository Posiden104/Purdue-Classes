.section        .data
.section        .text
.global         subString
subString:
        nop                 @no operation
        mov r7, $1          @exit syscall
        svc $0              @wake kernel
        .end
