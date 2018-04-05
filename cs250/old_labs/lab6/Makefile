
all: mystrlen compute subString bubble

mystrlen: mystrlen.o
	gcc -o mystrlen mystrlen.o

compute: compute.o
	gcc -o compute compute.o

subString: subString.o
	gcc -c main_subString.c -o main_subString.o
	gcc -o subString subString.o main_subString.o

bubble: bubble.o
	gcc -c main_bubble.c -o main_bubble.o
	gcc -o bubble bubble.o main_bubble.o

mystrlen.o: mystrlen.S
	gcc -c mystrlen.S -o mystrlen.o

compute.o: compute.S
	gcc -c compute.S -o compute.o

subString.o: subString.S
	gcc -c subString.S -o subString.o

bubble.o: bubble.S
	gcc -c bubble.S -o bubble.o


clean:
	rm -f *.out *.o mystrlen compute subString bubble
