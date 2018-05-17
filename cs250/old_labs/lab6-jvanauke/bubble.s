.section        .data
.section        .text
.global         bubble
bubble:
		push {r4-r8, fp, lr}	//push reg onto stack
		mov fp, sp				
		mov r4, r1
		mov r1, r0
		mov r0, r4

		cmp r1, #1				//if n <= 1
		ble end_outer			//exit
	
		sub r5, r1, #1			//set n-1 comp
		mov r4, r0				//init ptr
		mov r6, #0				//set flag

start_loop:
		ldr r7, [r4], #4		//load int
		ldr r8, [r4]			//load second int
		cmp r7, r8				//if r7 < r8, dont swap
		ble dont_swap			

		mov r6, #1				//flag = 1
		sub r4, r4, #4			//resetpointer
		swp r8, r8, [r4]		//swap value in r8 with addr in r4
		str r8, [r4, #4]!		//renew r8 with new addr

dont_swap:
		subs r5, r5, #1			//sub 1 from counter
		bne start_loop			//restart loop if r5 != 0
		
end_inner:
		cmp r6, #0				//check flag
		beq end_outer			//if 0, end

		mov r6, #0				//set flag = 0
		mov r4, r0				//reset ptr
		sub r5, r1, #1			//reset counter
		b start_loop			//goto loop

end_outer:
		mov sp, fp				//restore sp
		pop {r4-r8, fp, lr}		//restore reg r4-r8, fp and lr
		bx lr					// return

        mov r7, $1          @exit syscall
        svc $0              @wake kernel
        .end
