#include <stdlib.h>
#include <stdio.h>
#include <string.h>

int equivalent( char c1, char c2){
	return (c1<='Z' ? c1 : c1-('a'-'A') )  
		== (c2<='Z' ? c2 : c2-('a'-'A') ); 
}
int ignore( char c){
	return !( ('A'<=c&&c<='Z') || ('a'<=c&&c<='z') ); 
}
int isPal( char*str){
	int f=0;
	int l=strlen(str)-1;
	while( f<l){
		while( ignore(str[f]) ) f++; 
		while( ignore(str[l]) ) l--; 
		if( f>=l ) break; 
		if( equivalent( str[f],str[l] ) ){
			f++;
			l--;
		} else {
			return 0;
		}
	}
	return 1;
}

int main( int argc, char *argv[] ){
	if( argc!=2 ){
		printf("Error\n");
		exit(4);
	}
	if( isPal(argv[1]) ){
		printf("Yes\n");
	} else {
		printf("No\n");
	}
}
