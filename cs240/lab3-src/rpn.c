
#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <math.h>

#include "rpn.h"
#include "nextword.h"
#include "stack.h"


double rpn_eval(char * fileName, double x) {
	
	double a, b, c;
	FILE *fd;

	fd = fopen(fileName, "r");
	if(fd == NULL) {
		printf("Could not open file %s\n", fileName);
		exit(1);
	}

	char *word;
	double d;
	while((word = nextword(fd))!= NULL) {
		d = atof(word);
		if(strcmp(word, "+") == 0){
			double b = stack_pop();
			double a = stack_pop();
			double c = a + b;
			stack_push(c);
		} else if(strcmp(word, "-") == 0){
			double b = stack_pop();
			double a = stack_pop();
			double c = a - b;
			stack_push(c);
		} else if(strcmp(word, "/") == 0){
			double b = stack_pop();
			double a = stack_pop();
			double c = a / b;
			stack_push(c);
		} else if(strcmp(word, "*") == 0){
			double b = stack_pop();
			double a = stack_pop();
			double c = a * b;
			stack_push(c);
		} else if(strcmp(word, "sin") == 0){
			double a = stack_pop();
			double b = sin(a);
			stack_push(b);
		} else if(strcmp(word, "cos") == 0){
			double a = stack_pop();
			double b = cos(a);
			stack_push(b);
		} else if(strcmp(word, "pow") == 0){
			double b = stack_pop();
			double a = stack_pop();
			double c = pow(a, b);
			stack_push(c);
		} else if(strcmp(word, "log") == 0){ //natural logarithm
			double a = stack_pop();
			double b = log(a);
			stack_push(b);
		} else if(strcmp(word, "exp") ==0){
			double a = stack_pop();
			double b = exp(a);
			stack_push(b);
		} else if(strcmp(word, "x") == 0) {
			stack_push(x);
		} else {
			stack_push(d);
		}
	}

	if(stack_top() > 1) {
		printf("Elements remain in the stack\n");
		exit(1);
	}

	double result = stack_pop();
	return result;
}

