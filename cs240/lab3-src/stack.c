
#include <stdio.h>
#include "stack.h"
#include <stdlib.h>

int top=0;
double stack[MAXSTACK];

void stack_clear() 
{
  top = 0;
}

double stack_pop()
{
	if(top != 0) {
		return stack[top--];
	}
	printf("Stack underflow\n");
	exit(1);
}

void stack_push(double val)
{
	if(top != MAXSTACK) {
		stack[++top] = val;
	}
}

void stack_print()
{
	printf("Stack:\n");
	if(top == 0) {
		printf("Stack is empty");
	}
	for(int i = 1; i <= top; i++) {
		printf("%d: %.6f\n", i - 1, stack[i]);
	}
}

int stack_top()
{
  return top;
}

int stack_max()
{
  return MAXSTACK;
}

int stack_is_empty()
{
  return top == 0;
}


