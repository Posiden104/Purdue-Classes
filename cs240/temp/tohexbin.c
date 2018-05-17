#include <stdlib.h>
#include <stdio.h>

char *toBase( int base, int num){
	int size=0;
	int numTest = num;
	while( numTest>0){
		size++;
		numTest /= base;
	} 
	char *str = (char*)malloc(size*sizeof(char));
	int i=size-1;
	while(num>0){
		int r=num%base; 
		num /= base;
		if( 0<=r && r<=9 ){
			str[i]='0'+r;
		} else {
			str[i]='A'+r-10; 
		}
		i--;
	}
	return str;
}

int main( int argc, char *argv[] ){
	if( argc<2 ){
		printf("You must have an inupt number\n");
		exit(1);
	}
	int num = atoi( argv[1] );
	printf( "Binary: 0b%s\n",toBase(2,num) );
	printf("Hexadecimal: 0x%s\n", toBase(16,num) );
}
