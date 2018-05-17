.section        .data
first:	.word 0
second:	.word 0
op:		.space 10

num1:	.asciz "Enter the first number:"
num2:	.asciz "Enter the second number:"
askop:	.asciz "Enter the operator:"
inv:	.asciz "Invalid input\n"

print:	.asciz "Returned answer:%d\n"
format:	.asciz "%d"
fchar:	.asciz "%s"

add:	.word 43
sub:	.word 45
mult:	.word 42
div:	.word 47
.section        .text
addr1:	.word first
addr2:	.word second
aprint:	.word print
ainv:	.word inv

.global         main
main:
        ldr r0, =num1
		bl printf

		ldr r0, =format
		ldr r1, addr1
		bl scanf

		ldr r0, =askop
		bl printf
		
		ldr r0, =fchar
		ldr r1, =op
		bl scanf
		
		ldr r0, =num2
		bl printf

		ldr r0, =format
		ldr r1, addr2
		bl scanf

		ldr r0, =op
		ldr r0, [r0]
		
		ldr r1, =add
		ldr r1, [r1]
		cmp r0, r1
		beq madd

		ldr r1, =sub
		ldr r1, [r1]
		cmp r0, r1
		beq msub

		ldr r1, =mult
		ldr r1, [r1]
		cmp r0, r1
		beq mmult

		ldr r1, =div
		ldr r1, [r1]
		cmp r0, r1
		beq mdiv

		b end

madd:
		ldr r0, addr1
		ldr r1, addr2
		ldr r0, [r0]
		ldr r1, [r1]
		add r1, r0, r1
		
		b end

msub:
		ldr r0, addr1
		ldr r1, addr2
		ldr r0, [r0]
		ldr r1, [r1]
		sub r1, r0, r1

		b end

mmult:
		ldr r0, addr1
		ldr r1, addr2
		ldr r0, [r0]
		ldr r1, [r1]
		mul r1, r0, r1

		b end

mdiv:
		ldr r0, addr1
		ldr r1, addr2
		ldr r0, [r0]
		ldr r1, [r1]
		
		cmp r1, #0
		beq divz

		sdiv r1, r0, r1
		b end

divz:	
		ldr r0, ainv
		bl printf

		mov r0, $0
		bl fflush

		mov r7, $1          @exit syscall
        svc $0              @wake kernel

end:
		ldr r0, aprint
		bl printf
		
		mov r0, $0
		bl fflush


		mov r7, $1          @exit syscall
        svc $0              @wake kernel
        .end
