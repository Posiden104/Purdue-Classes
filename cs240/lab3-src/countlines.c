
#include <stdio.h>
#include <stdlib.h>

int
main(int argc, char ** argv) {
	int c;
	int lines=0;
	
	printf("Program that counts lines.\n");

	if(argc < 2) {
		printf("Usage: countlines filename\n");
		exit(1);
	}

	char * fileName = argv[1];
	FILE * fd = fopen(fileName, "r");
	if(fd == NULL) {
		printf("Could not open file %s\n", fileName);
		exit(1);
	}
	while((c = fgetc(fd)) != EOF){
		if(c == '\n'){
			lines ++;
		}
	}
	printf("Total lines: %d\n", lines);

	exit(0);
}
