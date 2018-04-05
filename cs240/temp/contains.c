#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

char *contains( char*hay, char*needle ){
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

int main(){
	char *h, *n;

	h="abcd"; n="b";
	printf("%s, %s\n", h, contains(h,n) );
	//assert( contains(h,n)-h == 1 );

	h="abcdefg"; n="efg";
	//assert( contains(h,n)-h == 4 );
	printf("%s, %s\n",h, contains(h,n) );

	h="lolwat"; n="haha";
	//assert( contains(h,n) == NULL );
	printf("%s, %s\n",h, contains(h,n) );

	h="that"; char *t="acsdk"; n="this";
	//assert( contains(h,n) == NULL );
	printf("%s\n",contains(h,n) );

	//h="aabbb"; n="abbbb";
	//assert( contains(h,n) == NULL );
}
