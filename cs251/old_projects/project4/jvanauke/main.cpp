#include <iostream>
#include <string>
#include <stdlib.h>
#include "ExternalSort.h"
#include "ExternalSearch.h"

using namespace std;

int main(int argc, const char* argv[])
{	
	if(argv[1] == string("1")) // Run P4 Part 1
	{
		extern_sort(string(argv[2]), string(argv[3]), string(argv[5]), atoi(argv[4]));
	}
	else if(argv[1] == string("2"))
	{
		int i = 3;
		while(argv[i] != "" && argv[i] != NULL){
			if(extern_search(argv[2], argv[i])){
				cout << "found" << endl;
			} else {
				cout << "missing" << endl;
			}
			i++;
		}
	}
	return 0;
}
