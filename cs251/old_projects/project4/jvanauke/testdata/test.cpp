#include <iostream>
#include <string>
#include <cstdlib>
#include <ctime>
using namespace std;

static const char alphanum[] = "abcdefghijklmnopqrstuvwxyz";

int stringLength = sizeof(alphanum) - 1;

char genRandom() {
	return alphanum[rand() % stringLength];
}

int main(int argc, const char* argv[]){
	int i = 0;
	int len = rand() % 21;
	len++;
	int size = 100000;
	if(!argc == 0) {
		size = atoi(argv[1]);
	}
	for(i = 0; i < size; i++){
		len = rand() % 21;
		len++;
		for(int z = 0; z < len; z++){
			cout << genRandom();
		}
		cout << endl;
	}
	return 0;
}
