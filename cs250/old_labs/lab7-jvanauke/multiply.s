.section	.data

.section	.text
.global		multiply
multiply:
	push {lr}
	mov r2, #1	@set counter to 1
	mov r1, r3

	cmp r0, #0
	blt fzero
	beq zero

	cmp r1, #0
	blt szero
	beq zero

	bl mult
	mov r0, r3
	pop {pc}

@r0 = loop num
@r1 = add num
@r2 = counter
@r3 = current total

mult:
	push {lr}
	
	cmp r2, r0	@ test to see if counter is maxed
	beq end1

	add r2, r2, #1	@ increase counter
	
	bl mult
	add r3, r3, r1	@ do the addition

	pop {pc}

fzero:
	cmp r1, #0
	blt bzero
	beq zero

	neg r0, r0
	bl mult
	
	mov r0, r3
	neg r0, r0
	
	b end1

szero:
	neg r1, r1
	mov r3, r1
	bl mult
	mov r0, r3
	neg r0, r0

	b end1

zero:
	mov r0, #0
	b end1

bzero:
	neg r0, r0
	neg r1, r1
	mov r3, r1
	bl mult
	mov r0, r3
	b end1

end1:
	pop {pc}

.end
