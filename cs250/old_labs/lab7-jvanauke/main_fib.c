/*
* CS250 lab 7
*
* Do not change this file
*/

#include <stdio.h>

extern int fibonacci(int n);

int main()
{
	int a;
	printf("Enter Fibonacci number:\n");
	scanf("%d", &a);
	printf("Fibonacci number:\n");
	printf("%d\n", fibonacci(a));
}



