#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int fib( int index){
	if( index<=0 ) return 0;
	if( index==1 ) return 1;
	if (index>1) 
		return fib(index-1) + fib(index-2);
	else 
		exit(2);
}

int main(){
	printf("Program Fibonacci\n");
	char *line = (char*) malloc(100*sizeof(char));
	int num;
	do {
		printf("Input k? ");
		fgets(line,100,stdin);
		num = atoi(line);
		printf("The %dth Fibbonacci number is %d\n",
			num, fib(num) );
		printf("Do you want to continue yes/no? ");
		fgets(line,100,stdin);
	} while ( 0==strcmp(line,"yes\n") );
}

