#include <stdio.h>

int main() {
  for(int i= 0; i < 11; i+=0.1) {
    if(i == 1) {
      printf("OK\n");
    } else {
      printf("not OK\n");
    }
  }
}
