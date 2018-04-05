/**
**CS250 lab6
**
**Do not change this file
**/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char const *argv[]) {
  printf("%s", "Enter the source string:");
  int x;
  char buffer1[2048];
  char buffer2[2048];
  int i = 0;
  while ((x = getchar())) {
    if(x == '\n' || x == '\0'){
      break;
    }else{
      buffer1[i++] = x;
    }
  }
  buffer1[i] = 0;
  printf("%s", "Enter the substring:");
  i = 0;
  while ((x = getchar())) {
    if(x == '\n' || x == '\0'){
      break;
    }else{
      buffer2[i++] = x;
    }
  }
  buffer2[i] = 0;
  char * out = subString(buffer1,buffer2);
  if(out == NULL){
    printf("%s\n", "Cannot find substring");
    return 0;
  }
  printf("Returned String:%s\n", out);
  return 0;
}
