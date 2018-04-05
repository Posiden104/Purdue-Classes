#include <stdio.h>
#include <stdlib.h>
#include "mystring.h"

// Type "man string" to see what every function expects.

int mystrlen(char * s) {
	int i = 0;
	while(s[i] != '\0'){
		i++;
	}
	return i;
}

char * mystrcpy(char * dest, char * src) {
	char * p = src;
	char * q = dest;
	while(*p != '\0'){
		*q = *p;
		p++;
		q++;
	}
	*q = '\0';
	return dest;
}

char * mystrcat(char * dest, char * src) {
	int i = mystrlen(dest);
	int j = 0;
	while(src[j] != '\0'){
		dest[i] = src[j];
		i++;
		j++;
	}
	dest[i] = '\0';
	return dest;
}

int mystrcmp(char * s1, char * s2) {
	int i = 0;
	
	while(s1[i] != '\0' || s2[i] != '\0') {
		if(s1[i] > s2[i]) {
			return 1;
		} else if(s1[i] < s2[i]) {
			return -1;
		}
		if(s1[i] == '\0' && s2[i] != '\0') {
			return -1;
		} else if(s2[i] == '\0' && s1[i] != '\0') {
			return 1;
		}
		i++;
	}
	return 0;
}

char * mystrstr(char * hay, char * needle) {
	char *h = hay;
	char *n = needle;
	char *hLoc = hay;
	while(*h != '\0') {
		if(*n == *h) {
			hLoc = h;
			while(*n == *h) {
				h++;
				n++;
				if(*n == '\0') {
					return hLoc;
				}
			}
		}
		h++;
		n = needle;
	}
	return NULL;
}

char * mystrdup(char * s) {
	int len = mystrlen(s);
	char *dup = (char *)malloc(len * sizeof(char));
	dup = mystrcpy(dup, s);
	return dup;
}

char * mymemcpy(char * dest, char * src, int n){
	char *p = dest;
	char *q	= src;
	while(q - src != n) {
		*p = *q;
		p++;
		q++;
	}
	return dest;
}

