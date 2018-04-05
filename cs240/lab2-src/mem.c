#include <stdio.h>
#include <string.h>
#include <stdlib.h>

void mymemdump(FILE *fd, char * p , int len);

struct X{
  char a;
  int i;
  char b;
  int *p;
};

struct List {
  char * str;
  struct List * next;
};

int
main() {
  char str[20];
  int a = 5;
  int b = -5;
  double y = 12;
  struct X x;
  struct List * head;

  x.a = 'A';
  x.i = 9;
  x.b = '0';
  x.p = &x.i;
  strcpy(str, "Hello world\n");
  printf("&str=0x%lX\n", (unsigned long)str);
  printf("&a=0x%lX\n", (unsigned long)&a);
  printf("&b=0x%lX\n", (unsigned long)&b);
  printf("&x=0x%lX\n", (unsigned long)&x.a);
  printf("&y=0x%lX\n", (unsigned long) &y);

  mymemdump(stdout, (char *) &x.a, 64);
  printf("head=0x%lx\n", (unsigned long) head);
  head = (struct List *) malloc(sizeof(struct List));
  head->str=strdup("Welcome ");
  printf("head->str=0x%lX\n", (unsigned long) head->str);
  head->next = (struct List *) malloc(sizeof(struct List));
  printf("head->next=0x%lX\n", (unsigned long) head->next);
  head->next->str = strdup("to ");
  printf("head->next->str=0x%lX\n", (unsigned long) head->next->str);
  head->next->next = (struct List *) malloc(sizeof(struct List));
  printf("head->next->next=0x%lX\n", (unsigned long) head->next->next);
  head->next->next->str = strdup("cs250");
  printf("head->next->next->str=0x%lX\n", (unsigned long) head->next->next->str);
  head->next->next->next = NULL;
  printf("head->next->next->next=0x%lX\n", (unsigned long) head->next->next->next);
  mymemdump(stdout, (char*) head, 128);
}

