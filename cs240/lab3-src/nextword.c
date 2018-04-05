#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>

//
// Separates the file into words
//

#define MAXWORD 200
char word[MAXWORD];
int wordLength;

// It returns the next word from fd.
// If there are no more more words it returns NULL. 
char * nextword(FILE * fd) {
  	int c;
	char * q = &word[wordLength];
	while((c = fgetc(fd)) != EOF){
		if(!isspace(c) && c != '\n') {
			word[wordLength] = c;
			wordLength ++;
		} else if(*q != '\n' && *q != '\0') {
			return q;
		}
	}
	
	if(*q != '\0') {
		return q;
	}

	// Return null if there are no more words
	return NULL;
}

