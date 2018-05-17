.section	.data
yesString:
	.asciz "yes"
promptString:
	.asciz "Play Music? -> "
formatString:
	.asciz "%s"
userInput:
	.space 128
testString:
	.asciz "value: %d\n"
totaltxt:
	.asciz "total %d\n"
test1:
	.asciz "num: %d\n"
testAvg:
	.asciz "average: %d\n"

.section	.text
.global main
main:
	@ This function call sets up wiringPi, just like Lab 5
	bl wiringPiSetup	@ wiringPiSetup()

	@ we need to call this next setup to use the ADC w/ SPI
	mov r0, #100
	mov r1, #0
	bl mcp3004Setup		@ mcp3004Setup(100, 0);

	b start

start:
	ldr r0, =promptString
	bl printf
	
	ldr r0, =formatString
	ldr r1, =userInput
	bl scanf		@ scanf("%s", userInput)
	
	ldr r0, =userInput
	ldr r1, =yesString
	bl strcmp 
	cmp r0, #0
	beq yes
	b no

yes:
	@ At this point, you can use analogRead(100) to read
	@ the value of the first ADC channel, 101 to read the second, etc

	mov r4, #0
	bl average1
	
	@b main

	@ we need to set up our GPIO pin modes to read from those pins
	@mov r0, #28
	@mov r1, #0
	@bl pinMode 		@ pinmode(28, 0)
        @ use digitalRead(28) to read from this GPIO pin (28)

	@ We've given you some code,
	@ you need to write the rest
	@ See lab 5 and 8 handouts if you're stuck on GPIO stuff
	@ Have fun :)

	b start
	
no:
	mov r7, #1
	swi 0

@r4 = i
@r1 = total

average1:
	push {lr}
	mov r1, #0
	b average

average:	
	mov r0, #100	@100 = first ADC channel
	push {r1-r3}
	bl analogRead	@analogRead(100)
	pop {r1-r3}

	@push {r0-r3}
	@mov r1, r0
	@ldr r0, =testString
	@bl printf
	@pop {r0-r3}

	add r1, r1, r0	@total	
	
	mov r0, #0x006A0 @100000 or .1 seconds
	orr r0, #0x18000

	push {r1-r3}
	bl usleep
	pop {r1-r3}

	@push {r0-r3}
	@ldr r0, =totaltxt
	@bl printf
	@pop {r0-r3}

	add r4, r4, #1
	cmp r4, #10
	blt average
	b decide

decide: 
	
	push {r0-r3}
	mov r1, r1
	ldr r0, =totaltxt
	bl printf
	pop {r0-r3}


	@decide the stuffs
	sdiv r1, r1, r4

	push {r0-r3}
	@mov r1, r0
	ldr r0, =testAvg
	bl printf
	pop {r0-r3}

	mov r0, #0x022	@high values = low light
	orr r0, #0x200
	cmp r1, r0		@546
	movgt r0, #0	@closing time
	push {r0-r3}
	blgt songify
	pop {r0-r3}
	cmp r1, r0		@546
	popgt {pc}

	cmple r1, #0x16C @364
	movgt r0, #1	@evening hours
	push {r0-r3}
	blgt songify
	pop {r0-r3}
	cmp r1, r0		@546
	popgt {pc}

	movle r0, #2	@active hours
	push {r0-r3}
	blle songify
	pop {r0-r3}

	pop {pc}	@return to yes

.end
