#include <stdio.h>
#include <stdlib.h>
#include <string.h>

double rpn( char *str){
	char **tokens = (char**) malloc(sizeof(void*)*strlen(str) );
	int tokc=0;
	char*last=str;
	while( *str!='\0' ) { 
		if( *str==' ' ){
			*str='\0';
			tokens[tokc]=last;
			tokc++;
			str++;
			while( *str==' ')
				str++;
			last=str;
		}
		str++;
	}
	tokens[tokc]=last; 
	tokc++;		

	double*stack = (double*)malloc(tokc*sizeof(double));
	for( int i=0; i<tokc; i++){ 
		if( 0==strcmp(tokens[i],"+") ){
			stack--;
			*stack=*stack + *(stack+1);
		} else if( 0==strcmp(tokens[i],"-") ){
			stack--;
			*stack=*stack - *(stack+1);
		} else if( 0==strcmp(tokens[i],"*") ){
			stack--;
			*stack=*stack * *(stack+1);
		} else if( 0==strcmp(tokens[i],"/") ){
			stack--;
			*stack=*stack / *(stack+1);
		} else {
			stack++;
			*stack=atof(tokens[i]);
		}
	}
	return *stack;
}

int main( int argc, char *argv[] ){
	printf("%f\n", rpn(argv[1]) );
}
