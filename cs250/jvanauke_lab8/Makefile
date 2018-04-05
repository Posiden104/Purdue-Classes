
all: main

main: arm main.s process.c
	gcc -o main main.o process.c -lwiringPi

arm: main.o
	as -o main.o main.s

clean:
	rm main
	rm main.o
