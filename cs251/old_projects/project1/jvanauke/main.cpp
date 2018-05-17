/**
 *  CS251 - Spring 2016: Project 1 Solution
 */

#include <iostream>
#include <iomanip>
#include <stdio.h>

using namespace std;

int main(int argc, char** argv)
{
	//FILE *inputfile = stdin;
	//FILE *outputfile = stdout;
	
	int line;

	cin >> line;

	cout << setfill('-') << setw((6 * line) - 3) << '-' << endl;

    	int i = 0;
	int next;
	while(cin>>next){
		i++;
		cout << setw(3) << setfill(' ') << next;
		if(i != line) cout << " | ";
		else {
			cout << endl;
			i = 0;
		}
	}
	
	cout << setfill('-') << setw((6 * line) -3) << '-' << endl;
	
	/* Close files if using C like notation */
	//fclose(inputfile);
	////fclose(outputfile);

    	/* Exit the program */
    	return 0;
}
