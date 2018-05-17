.section        .data
.balign 4
string: .skip 100
sub:	.skip 100
ptr:	.space 100
nsub:	.word 0
nstr:	.word 0
nptr:	.word 0
fourth:	.space 100
fifth:	.space 100
return: .word 0
print:	.asciz "string %c \nsub %c\n"
prnt:	.asciz "this char: %c\n"
founds:	.asciz "loop started\n"

.section        .text
.balign 4
afor:	.word fourth
afif:	.word fifth
astr:	.word string
asub:	.word sub
aptr:	.word ptr
aret:	.word return
ansub:	.word nsub
anstr:	.word nstr
anptr:	.word nptr

.global         subString

found:
	ldr r0, aptr
	ldr r0, [r0]
	b finish

notFound:
	mov r0, #0x00			// set r2 to be null
	b finish			// goto end

finish:
	ldr lr, aret
	ldr lr, [lr]

	ldr r4, afor
	ldr r5, afif
	ldr r4, [r4]
	ldr r5, [r5]

	bx lr				//return    
	
	mov r7, $1          @exit syscall
    svc $0              @wake kernel

find_sub1:
	ldr r0, anstr		//set r3
	ldr r0, [r0]		//to be
	ldr r3, astr		//pointer
	ldr r3, [r3]		//to
	add r3, r3,	r0		//string

	ldr r5, anptr		//save number we jumped
	str r0, [r5]

	ldr r0, aptr		//save location if this is not substring
	str r3, [r0]		//store pointer in memory

	ldr r2, asub		//load pointer to ptr to sub
	ldr r2, [r2]		//load ptr to sub
	mov r4, r2			//copy the pointer to sub into r4
	ldrb r2, [r2]		//load first char in sub into r2
	
	b find_sub

find_sub:
	// check against #0x00 for end of string
/*	
	ldr r0, =founds
	bl printf
*/

	ldr	r5, anstr			//load pos of str
	ldr r0, [r5]			
	add r0, r0, $1			//inc ^
	str r0, [r5]			//store ^
	ldr r3, astr			//load string
	ldr r3, [r3]			//ptr to str
	add r3, r3, r0			//move ptr to str

	ldr r5, ansub			//load pos of sub
	ldr r0, [r5]
	add r0, r0, $1			//inc ^
	str r0, [r5]			//store ^
	ldr r4, asub			//load sub
	ldr r4, [r4]			//ptr to sub
	add r4, r4, r0			//move ptr of sub

	ldrb r1, [r3]			//load char of string 
	ldrb r2, [r4]			//load char of sub
	
	// r1 = string char
	// r2 = sub char

	cmp r2, #0x00			//check for end of sub
	beq found				//end of the sub, we found substring
	
	cmp r1, #0x00			// check for end of string
	beq notFound

	cmp	r1, r2				//compare
	bne	reset				//branch not equal
	beq find_sub

subString:
	ldr r2, aret
	str lr, [r2]		//save return location

	ldr r2, afor		//save r4 and r5
	str r4, [r2]
	ldr r2, afif
	str r5, [r2]

	mov r4, $0
	ldr r5, ansub
	str r4, [r5]
	ldr r5, anstr
	str r4, [r5]
	ldr r5, anptr
	str r4, [r5]

	mov r2, r1			//move the args
	mov r1, r0
	
	ldr r3, astr		//save the args
	ldr r4, asub
	str r1, [r3]
	str r2, [r4]

	mov r3, r1			//store ptr to string in r3
	mov r4, r2			//store ptr to subin r4

	ldrb r1, [r1]		//set r1 to first char of string
	ldrb r2, [r2]		//set r2 to first char of sub

	ldr r0, aptr		//load return pointer
	ldr r5, astr
	ldr r5, [r5]
	str r5, [r0]		//store pointer in memory

	cmp r1, r2			//compare first chars
	beq find_sub1		//branch if equal
	
	b reset

reset:
	ldr r2, asub		//load pointer to ptr to sub
	ldr r2, [r2]		//load ptr to sub
	mov r4, r2			//copy the pointer to sub into r4
	ldrb r2, [r2]		//load first char in sub into r2
	
	ldr r3, aptr		//load ptr to ptr to prev start into r3
	ldr r3, [r3]		//set r3 to ptr to prev start loc

	ldr r5, anptr
	ldr r0, [r5]
	ldr r5, anstr
	str r0, [r5]

	ldr r0, ansub
	mov r5, $0
	str r5, [r0]

	b loop 

loop:
	ldr r5, anstr
	ldr r0, [r5]
	add r0, r0, $1
	str r0, [r5]
	
	ldr r3, astr
	ldr r3, [r3]
	add r3, r3, r0		//increase ptr for string(r3)

	ldrb r1, [r3]		//set r1 = char dereferenced by r3
	cmp r1, #0x00
	beq notFound

	/*mov r4, r1
	mov r5, r2
	ldr r0, =print
	bl printf

	mov r1, r4
	mov r2, r5
*/
	cmp r1, r2			//is char at r0 = first char in sub?
	beq find_sub1		//if so, go to find_sub
	b loop				//else, loop















useful:

/*	
	ldr r1, [r1]		//print first letter
	ldr r2, [r2]
	ldr r0, =print
	bl printf
	
	ldr r1, astr
	ldr r2, asub
	ldr r1,[r1]
	ldr r2,[r2]
	ldr r1, [r1]
	ldr r2, [r2]

	ldr r0, =print
	bl printf
	b notFound
*/	


.end
