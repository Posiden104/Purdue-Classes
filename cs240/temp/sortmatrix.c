#include <stdio.h>
#include <string.h>
#include <stdlib.h>

typedef struct Row {
	double *vals;
	double sum;
} Row;

Row *getMatrix( int *rows, int *cols){
	char *line = (char*) malloc(1000*sizeof(char));
	
	fgets(line,1000,stdin);
	char*p=line;
	while(*p!=' ') p++;
	*p = '\0';
	int m=atoi(line); 
	int n=atoi(p+1); 

	Row *matrix = (Row*) malloc(m*sizeof(Row));
	for( int i=0; i<m; i++ ){ 
		matrix[i].vals = (double*) malloc(n*sizeof(double));

		fgets(line,1000,stdin);
		int j=0;
		char*last=line;
		for( int k=0; k<=strlen(line); k++){
			if( line[k]==' ' ||  line[k]=='\0' ){
				line[k] = '\0';
				matrix[i].vals[j]=atof(last); 
				last=line+k+1; 
				j++;
			}
		}
		matrix[i].sum=0;
		for( int k=0; k<n; k++){
			matrix[i].sum += matrix[i].vals[k];
		}
	}
	free(line);
	*rows = m;
	*cols = n;
	return matrix;
}

void printMat( Row*mat, int m, int n){
	for( int i=0; i<m; i++){
		for( int j=0; j<n; j++){
			printf("%f ", mat[i].vals[j]);
		}
		printf("\n");
	}
}

int main(){
	int rows;
	int cols;
	Row * matrix = getMatrix(&rows,&cols);
	
	printf("Original Matrix:\n%d %d\n", rows, cols);
	printMat(matrix,rows,cols);
	
	for( int i=1; i<rows; i++){
		for( int j=0; j<cols-i; j++){
			if(matrix[j].sum>matrix[j+1].sum){
				Row sto = matrix[j];
				matrix[j] = matrix[j+1];
				matrix[j+1]=sto;
			}
		}
	}

	printf("Sorted Matrix:\n%d %d\n", rows, cols);
	printMat( matrix,rows,cols);

}
