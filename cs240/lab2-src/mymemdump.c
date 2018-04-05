#include <stdio.h>
#include <string.h>
#include <stdlib.h>
void mymemdump(FILE * fd, char * p , int len) {
 int i;
 fprintf(fd, "0x%016lX: ", (unsigned long) p); // Print address of the beginning of p. You need to print it every 16 bytes
 
 for (i=0; i <= len; i++) {
  if((i % 16 == 0 && i != 0)) {
    fprintf(fd, " "); // adds formatting spce
    for(int j = 0; j < 16; j++) { // cycles through the previous bytes
     int c = p[i - 16 + j] & 0xFF; // grabs the correct placement and masks to 1 byte
     fprintf(fd, "%c", (c >= 32 && c <= 127) ? c : '.'); // filters values to print correct text
    }
    if(i >= len) {
     break;
    }
    fprintf(fd, "\n");
    fprintf(fd, "0x%016lX: ", (unsigned long) &p[i]);
  }

  if(len - i < 16 && len % 16 != 0) {
   if(len > 16 && i >= 16 || len < 16) {
    //fprintf(fd, "\n here \n i: %d\n len: %d\n", i, len);
    int lenLeft = len - i;
    for(int j = i; j <= len; j++) {
     if(j == len) {
      fprintf(fd, "%*c", 49 - (lenLeft*3), ' ');
      for(int k = i; k < len; k++) {
       int c = p[k] & 0xFF;
       fprintf(fd, "%c", (c >= 32 && c <= 127) ? c : '.');
      }	
     } else {
       int c = p[j] & 0xFF;
       fprintf(fd, "%02X ", c);
     } 
    }
    i = len;
    break;
   }
  }

   int c = p[i] & 0xFF; // Get value at [p]. The &0xFF is to make sure you truncate to 8bits or one byte.

   // Print first byte as hexadecimal
   fprintf(fd, "%02X ", c);
 }

 fprintf(fd, "\n");
}
