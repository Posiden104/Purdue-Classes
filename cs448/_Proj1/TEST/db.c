#include <stdio.h>

int menu();

int main() {
	switch(menu()){
		case 1:
			break;
		case 2:
			break;
		case 3:
			break;
	}

	return 0;
}


int menu(){
	printf("Welcome user.\n");
	printf("\n");
	printf("What would you like to do?\n\n");
	printf("1. INSERT\n");
	printf("2. UPDATE\n");
	printf("3. DELETE\n");

	gets(num);
	int dec = 0;
	dec = dec * 10 + ( num[i] - '0' );
	printf("%d", dec);
	return dec;
}
