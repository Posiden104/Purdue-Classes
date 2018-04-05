.data
string:	.space 100
scanFormat:	.asciz "%s"
empty:	.asciz "%d/n"

strlen: .word 0		//word = int
return: .word 0		//word = int

.text

end:
	LDR LR, =return
	LDR LR, [LR]
	BX LR				//return

finish:
	LDR R1, =strlen
	LDR R1, [R1]
	LDR R0, =empty
	BL printf			//printf(ans)

	B end				// GOTO end

func:
	LDR R2, [R1]		//R1 holds ptr to string, make R2 hold current char in string
	CMP R2, #0
	BEQ finish			//If we found the end of the string, got to finish

	ADD R0, R0, #1		//R0 holds number of chars, add 1 to it
	ADD R1, R1, #1		//move pointer
	STR R0, [R3]		//store int strlen into memory that =strlen is pointing to

	B func				// loop

.gobal main
main:
	LDR R1, =return
	STR LR, [R1]
	
	LDR R0, =scanFormat
	LDR R1, =string
	BL scanf

	LDR R1, =string
	LDR R0, =strlen
	LDR R0, [R0]		//De-reference (makes R0 hold the actual int value of strlen)
	LDR R3, =strlen		//keep track of ptr to strlen int
	BL func
