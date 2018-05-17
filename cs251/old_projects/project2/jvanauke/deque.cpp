#include <iostream>
#include "deque.h"

using namespace std;

void Deque::print() {
	cout << "Print:" << endl;
	dNode* n = head;
	while(n != NULL) {
		cout << n->data << " ";
		n = n->next;
	}
	cout <<endl;
}

Deque::Deque()
{
	//NOTE: Start with an array of size 10!
	head = NULL;
	tail = NULL;
	allocated = 10;
	nElements = 0;
}

void Deque::enqueue_front(int n)
{
	if(is_full()) reallocate();
	dNode* tmp = new dNode;
	tmp -> data = n;
	tmp -> prev = NULL;
	if(head == NULL) {
		head = tail = tmp;
		tmp -> next = NULL;
	} else {
		tmp -> next = head;
		head = tmp;
	}
	nElements++;
	//print();
}

void Deque::enqueue_back(int n)
{
	if(is_full()) reallocate();
	dNode* tmp = new dNode;
	tmp -> data = n;
	tmp -> next = NULL;
	if(!tail) {
		head = tail = tmp;
		tmp -> prev = NULL;
	} else {
		tmp -> prev = tail;
		tail = tmp;
	}
	nElements++;
	//print();
}

int Deque::dequeue_front()
{
	int ret = head -> data;
	dNode* tmp = head;
	head = tmp -> next;
	//head -> prev = NULL;
	nElements--;
	free(tmp);
	//print();
	return ret;
}

int Deque::dequeue_back()
{
	int ret = tail -> data;
	dNode* tmp = tail;
	tail = tail -> prev;
	
	nElements--;
	free(tmp);
	//print();
	return ret;
}

bool Deque::is_full()
{
	if(nElements >= allocated) {
		return true;
	} else {
		return false;
	}
}

void Deque::reallocate()
{
	allocated *= 2;
}

int Deque::get_array_size() {
	return allocated;
}
