 #include <iostream>
 #include <utility>
 
using namespace std;

typedef struct Node {
	pair<int,int> p;
	Node* next;
}Node;

class Stack
{
 public:

	Stack();
	void push(pair<int,int>);
	pair<int,int> pop();
	int nElements;
 private:
	
	Node* head;
};
