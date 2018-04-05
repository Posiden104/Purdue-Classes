/**
* CS250 lab 7
* 
* Do not change this file
*
**/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern void quicksort(int* array, int size);

int main(int argc, char const *argv[]) {
  int size;
  printf("%s", "Enter the length of your array:");
  scanf("%d", &size);
  int i;
  int array[size];
  printf("%s\n", "Enter the numbers in your array:");
  for(i = 0; i < size; i++){
    scanf("%d", &array[i]);
  }
  quicksort(array, size);
  printf("%s\n", "After quicksort:");
  for(i = 0; i < size; i++){
    printf("%d\n", array[i]);
  }
  return 0;
}
