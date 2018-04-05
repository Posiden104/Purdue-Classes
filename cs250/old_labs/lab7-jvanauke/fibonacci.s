.section	.data

.section	.text
.global		fibonacci
fibonacci:
	push {lr}
	bl fib
	pop {pc}

fib:
	push {r1-r3, lr}

	cmp r0, #0	@compare r0 to 0
	beq end1	@return

	cmp r0, #1	@compare r0 to 1
	beq end1	@return
	
	mov r2, r0
	sub r0, r2, #1
	bl fib
	
	mov r1, r0
	sub r0, r2, #2
	bl fib

	add r0, r0, r1
	
	pop {r1-r3, pc}

end1:
	pop {r1-r3, pc}

.end
