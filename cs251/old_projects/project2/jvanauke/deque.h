#include <iostream>

using namespace std;

typedef struct dNode {
	int data;
	dNode* prev;
	dNode* next;
}dNode;

class Deque
{
	public:
		Deque();
		void enqueue_front(int);
		void enqueue_back(int);
		int dequeue_front();
		int dequeue_back();
		int get_array_size();
		
	private:
		dNode* head;
		dNode* tail;
		void reallocate();
		bool is_full();
		int nElements;
		int allocated;
		void print();
};
