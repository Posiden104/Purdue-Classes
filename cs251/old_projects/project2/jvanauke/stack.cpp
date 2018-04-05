 #include "stack.h"
 #include <utility>
 

 Stack::Stack()
 {
	head = NULL;	 
 }
 
 void Stack::push(pair<int,int> p)
 {
	 Node *n = new Node();
	 n->p = p;
	 n->next = head;
	 head = n;
	 nElements++;
 }
 
 pair<int,int> Stack::pop()
 {
	 if(nElements == 0) {
	 	return pair<int,int>(NULL,NULL);
	 }
	 Node *n = head;
	 pair<int,int> ret = n->p;
	 head = n->next;
	 nElements--;
	 return ret;
 }
