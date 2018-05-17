
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "slist.h"

void test1()
{
	SLList list;
}

void test2()
{
	SLList list;
	//printf("remove 2\n");
	//sllist_remove(&list, 2);
	//sllist_print(&list);
	
}

void test3()
{
	SLList list;
}

int
main(int argc, char ** argv) {

  	char * test;
  
  	if (argc <2) {
    		printf("Usage: test_slist test1|test2|test3|test4|\n");
   		exit(1);
  	}
  
  	test = argv[1];
  	printf("Running %s\n", test);
  	if (strcmp(test, "test1")==0) {
    		test1();
  	}
  	else if (strcmp(test, "test2")==0) {
    		test2();
  	}
  	else if (strcmp(test, "test3")==0) {
    		test3();
  	}
  	else if (strcmp(test, "test4")==0) {
    		test4();
  	}
	else {
		printf("Wrong test");
	}
}
