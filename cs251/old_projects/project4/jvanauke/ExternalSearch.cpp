#include <fstream>
#include "ExternalSearch.h"

fstream file;
int numChar;
int high;
int low;
int cur;

void findLine(){
	while(file.good()){
		char c;
		c = file.peek();
		if(c != '\n' && cur > 0 && c != EOF){
			file.seekg(-1, file.cur);
			cur--;
			//cout << c << endl;
		} else {
			if(cur != 0){
				file.seekg(1, file.cur);
				cur++;
			}
			return;
		}
	}
}

bool extern_search(string fileName, string toFind)
{
	int lim = 100;
	file.open(fileName, fstream::in);
	if(!file.is_open()) return false;

	file.seekg(0, file.end);
	numChar = file.tellg();
	file.seekg(0, file.beg);
	
	low = file.tellg();
	high = numChar;
	cur = (low + high) / 2;
	
	bool stop = false;

	while(!stop && lim > 0){
		if(low == high){
			stop = true;
		}

		string tmp;

		file.seekg(cur, file.beg);
		findLine();
		
		//cout << "at " << file.tellg() << " ";
		getline(file, tmp);
		//cout << "current guess: " << tmp << endl;

		if(tmp.compare("") == 0){
			file.close();
			return false;
		}

		int n = toFind.compare(tmp);
		if(n == 0){
			file.close();
			return true;
		}
		else if(n < 0){
			high = cur;
			cur = (low+high)/2;
		} else if(n > 0){
			low = cur;
			cur = (low+high)/2;
		}
		if(file.tellg() == high){
			stop = true;
		}
		lim--;
	}

	file.close();
	return false;
}
