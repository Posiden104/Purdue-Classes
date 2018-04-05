#include <stdio.h>
#include <stdlib.h>
#include <math.h>

void printLine(int x, double y, double lx) {
  printf("<%3d>-%.02f:%.02f", x, y, lx);
}

double hn(int k) { 
  double result = 0;
  for(double i = 1; i < k + 1; i += 1) {
    result += (1/i);
  }
  return result;
}

double lx(double k) {
 double result = 2 * log10(k) + 1;
 return result;
}

int main() {
 printf("Key: < xvalue>-harmonic sum: 2*log(n)+1\n");
 
 int j = 0;
  for(int i = 1; i < 1001; i += 10) {
    printLine(i, hn(i), lx(i));
    if(j == 5) {
      printf("\n");
      j = 0;
    } else {
      printf(" || ");
      j++;
    }
  }
  j == 0;
  printf("\n\nKey: __\n");
  for(int i = 0; i < 1001; i += 10) {
    printLine(i, log(i), i*log(i));
    if(j == 5) {
      printf("\n");
      j = 0;
    } else {
      printf(" || ");
      j++;
    }
  }

}


