#include <iostream>
#include "stack.h"
#include "deque.h"

using namespace std;



void read_part_1()
{
	//Read the number of commands to execute
    int n;
    cin >> n;
    
    // Init stack
    Stack stack = *(new Stack());
    
    char command1;
    int value1,value2;
    
    while (n--) 
    {
        cin >> command1;
        if (command1 == 'i')
        {
            cin >> value1 >> value2;
            
            // push the values into stack
	    stack.push(pair<int,int>(value1,value2));
        }
        if (command1 == 'p')
        {
            // pop and pring values
	    if(stack.nElements != 0) {
	    	pair<int,int> p = stack.pop();
		cout << p.first << " " << p.second << endl;
	    } else {
		cout << "empty" << endl;
	    }
        }
    }
    delete &stack;
}

bool search(char puzzle[], int i, int j, int col, int row, string::iterator it, string::iterator itEnd, string word, Stack* stack) {
	it++;
	if(it == itEnd) {
		return true;
	}
	//TL
	if(i != 0 && j != 0){
		if(puzzle[(i-1)*row + j-1] == *it) {
			stack->push(pair<int, int>(i-1,j-1));
			if(search(puzzle, i-1, j-1, col, row, it, itEnd, word, stack)) return true;
		}
	}
	//T
	if(i != 0){
		if(puzzle[(i-1)*row + j] == *it) {
			stack->push(pair<int, int>(i-1,j));
			if(search(puzzle, i-1, j, col, row, it, itEnd, word, stack)) return true;
		}
	}
	//TR
	if(i != 0 && j != col){
		if(puzzle[(i-1)*row + j+1] == *it) {
			stack->push(pair<int, int>(i-1,j+1));
			if(search(puzzle, i-1, j+1, col, row, it, itEnd, word, stack)) return true;
		}
	}
	//L
	if(j != 0){
		if(puzzle[i*row + j-1] == *it) {
			stack->push(pair<int, int>(i,j-1));
			if(search(puzzle, i, j-1, col, row, it, itEnd, word, stack)) return true;
		}
	}
	//R
	if(j != col){
		if(puzzle[i*row + j+1] == *it) {
			stack->push(pair<int, int>(i,j+1));
			if(search(puzzle, i, j+1, col, row, it, itEnd, word, stack)) return true;
		}
	}
	//BL
	if(i != row && j != 0){
		if(puzzle[(i+1)*row + j-1] == *it) {
			stack->push(pair<int, int>(i+1,j-1));
			if(search(puzzle, i+1, j-1, col, row, it, itEnd, word, stack)) return true;
		}
	}
	//B
	if(i != row){
		if(puzzle[(i+1)*row + j] == *it) {
			stack->push(pair<int, int>(i+1,j));
			if(search(puzzle, i+1, j, col, row, it, itEnd, word, stack)) return true;
		}
	}
	//BR
	if(i != row && j != col){
		if(puzzle[(i+1)*row + j+1] == *it) {
			stack->push(pair<int, int>(i+1,j+1));
			if(search(puzzle, i+1, j+1, col, row, it, itEnd, word, stack)) return true;
		}
	}
	
	stack->pop();
	it--;
	return false;
}

void read_part_2()
{
	int num_rows,num_cols;
	Stack stack = *(new Stack());

	//Read in the number of rows and cols in the puzzle
	cin >> num_rows;
	cin >> num_cols;

	//Declare the word search array
	char puzzle[num_rows*num_cols];

	//Fill in the array
	for(int i = 0; i < num_rows; i++){
	  for(int j = 0; j < num_cols; j++) {
		cin >> puzzle[i*num_rows + j];
	  }
	}
	//Read in the word we're looking for
	string word;
	cin >> word;
	
	//Part 1b: Run the word search
	bool finished = false;
	string::iterator it = word.begin();
	string::iterator itEnd = word.end();
	for(int i = 0; i < num_rows; i++) {
	  for(int j = 0; j < num_cols; j++) {
		if(puzzle[i*num_rows + j] == *it) {
			stack.push(pair<int,int>(i,j));
			finished = search(puzzle, i, j, num_cols, num_rows, it, itEnd, word, &stack);
		}
		if(finished) break;
	  }
	  if(finished) break;
	}

	if(finished) {
		pair<int, int> p;
		Stack s2 = *(new Stack());
		while(stack.nElements != 0) {
			s2.push(stack.pop());
		}
		while(s2.nElements != 0) {
			p = s2.pop();
			cout << p.first << " " << p.second << endl;
		}
	} else {
		cout << "not found" << endl;
	}
}

void read_part_3()
{
	//Read in the number of commands to execute
	Deque deque = *(new Deque);
	int n;
	cin >> n;
	char ed,fb;
	int data;
	while(n--){
	  cin >> ed;
	  if(ed != 's') {
	  	cin >> fb;
	  	//cout << "arg: " << ed << " "  << fb;
		if(ed == 'e') {
	  		cin >> data;
		//	cout << " " << data << endl;
	  		fb == 'f' ? deque.enqueue_front(data) : deque.enqueue_back(data);
	  	} else if(ed == 'd') {
		//	cout << endl;
	  		data = (fb == 'f' ? deque.dequeue_front() : deque.dequeue_back());
	  		cout << data << endl;
	  	}
	  } else {
	  	cout << deque.get_array_size() << endl;
	  }
	}
}

void read_input()
{
	int project_part;
	//Read the first line of the test case
	cin >> project_part;
	
	switch(project_part)
	{
		case 1:
			read_part_1();
			break;
		case 2:
			read_part_2();
			break;
		case 3:
			read_part_3();
			break;
	}
}

int main()
{
	// Read in the test case
	read_input();
	return 0;
}
