/**
**CS250 lab6
**
**Do not change this file
**/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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
  bubble(size,array);
  printf("%s\n", "After bubble sorting:");
  for(i = 0; i < size; i++){
    printf("%d\n", array[i]);
  }
  return 0;
}