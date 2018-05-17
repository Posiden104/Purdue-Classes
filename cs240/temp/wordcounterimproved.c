
#include <stdio.h>
#include <string.h>

int main(){
	char line[1000];
	fgets(line,1000,stdin);
	char *words[1000];
	int wordc=0; int totalc=0;
	int counts[1000];
	char *last=line; 
int totalLen = strlen(line); 
		for( int i=0; i<totalLen; i++){
		if( line[i]==' ' ){
			line[i]='\0';
			i++; 
			int found=0;
			for( int j=0; j<wordc; j++){ 
				if(0==strcmp(last,words[j]) ){
					counts[j]++;
					found=1;
				}
			}
			if(!found){ 
				words[wordc]=last;
				counts[wordc]++;
				wordc++;
			}
			while ( line[i]==' ' ){
				i++; 
			}
			last = &line[i];
			totalc++;
		}
	}

	printf("Histogram\n====================\n");
	for( int i=0; i<wordc; i++){
		printf("%s    %d\n", words[i], counts[i] );
	}
	printf( "Total Words:%d\nTotal Different words:%d\n", totalc, wordc );

	//NAME SORT:
	for( int i=1; i<wordc; i++){ 
		for( int j=0; j<wordc-i; j++){
			if( strcmp(words[j],words[j+1])>0 ){
				char *sto=words[j]; 
				words[j]=words[j+1];
				words[j+1]=sto;
				int isto=counts[j]; 
				counts[j]=counts[j+1];
				counts[j+1]=isto;
			}
		}
	}
	printf("Histogram by name\n====================\n");
	for( int i=0; i<wordc; i++){
		printf("%s    %d\n", words[i], counts[i] );
	}
	printf( "Total Words:%d\nTotal Different words:%d\n", totalc, wordc );

	//COUNT SORT: 
	for( int i=1; i<wordc; i++){
		for( int j=0; j<wordc-i; j++){
			if( counts[j]<counts[j+1] ){
				char *sto=words[j]; 
				words[j]=words[j+1];
				words[j+1]=sto;
				int isto=counts[j]; 
				counts[j]=counts[j+1];
				counts[j+1]=isto;
			}
		}
	}
	printf("Histogram by count\n====================\n");
	for( int i=0; i<wordc; i++){
		printf("%s    %d\n", words[i], counts[i] );
	}
	printf( "Total Words:%d\nTotal Different words:%d\n", totalc, wordc );
}
