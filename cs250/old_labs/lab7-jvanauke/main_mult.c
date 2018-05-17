/**
* CS250 lab 7
* 
**/

#include <stdio.h>

extern int multiply(int m, int n);

int main()
{
	int m, n;
	printf("Enter first integer:\n");
	scanf("%d", &m);
	printf("Enter second integer:\n");
	scanf("%d", &n);
	printf("Output:\n");
	printf("%d\n", multiply(m, n));
}



