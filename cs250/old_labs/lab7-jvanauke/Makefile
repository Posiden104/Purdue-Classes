CC=gcc

all: clean fibonacci multiply quicksort
	
fibonacci: 
	$(CC) -g -o fibonacci main_fib.c fibonacci.s

multiply:
	$(CC) -g -o multiply main_mult.c multiply.s

quicksort:
	$(CC) -g -o quicksort quicksort.s main_quicksort.c

clean:
	rm -rf quicksort fibonacci multiply
