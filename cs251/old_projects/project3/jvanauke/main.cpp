#include <iostream>
#include <cstdlib>
#include <cstring>

#include "heap.h"

using namespace std;

int* T1 = new int[1];
int* T2 = new int[1];
int maxLoop;
BinaryHeap heap = new BinaryHeap();

void insert(int x);

int find_hash(int x, int table) {
    // % is mod
    if(table == 1) {
	return x % len;
    } else if(table == 2) {
	return x % (len - 1);
    }
}

int lookup(int x) {
    int h = find_hash(x, 1);
    if(T1[h] == x) return 1;
    h = find_hash(x, 2);
    if(T2[h] == x) return 2;
    return 0;
}

void table_clear(int* table) {
    for(int i = 0; i < len; i++) {
	table[i] = -1;
    }
}

void rehash() {
    // Double len
    len = len * 2;
    int* t1 = T1;
    int* t2 = T2;
    T1 = new int[len];
    T2 = new int[len];

    table_clear(T1);
    table_clear(T2);

    int i = 0;

    while(i < len / 2) {
    	if(t1[i] != -1 && t1[i] != 0) 
  	    insert(t1[i]);
    	int i2 = i;
    	if(t2[i2] != -1 && t2[i2] != 0)
    	    insert(t2[i2]);
	i++;
    }
    delete(t1);
    delete(t2);
}

void remove(int x) {
    int p = lookup(x);
    if(p == 1){
    	T1[find_hash(x, 1)] = -1;
	cout << x << " out T1[" << find_hash(x, 1) << "]" << endl;
    } else if(p == 2){
	T2[find_hash(x, 2)] = -1;
	cout << x << " out T2[" << find_hash(x, 2) << "]" << endl;
    } else if(!p) {
	cout << "no out " << x << endl;
    }
    return;
}

void insert(int x){
    int i = lookup(x);
    if(i == 1){
    	cout << x << " already in T1[" << find_hash(x, 1) << "]" << endl;
    	return;
    } else if(i == 2){
	cout << x << " already in T2[" << find_hash(x, 2) << "]" << endl;
	return;
    }
    int temp;

    i = 0;

    while(i < maxLoop) {
	if(x == 0) return;
	int h = find_hash(x, 1);
	if(T1[h] == -1 || T1[h] == 0) {
	    T1[h] = x;
	    cout << x << " in T1[" << h << "]!" << endl;
	    return;
	} else {
	    temp = T1[h];
	    T1[h] = x;
	    cout << x << " in T1[" << h << "]" << endl;
	    x = temp;
	}
	h = find_hash(x, 2);
	if(T2[h] == -1 || T2[h] == 0) {
	    T2[h] = x;
	    cout << x << " in T2[" << h << "]!" << endl;
	    return;
	} else {
	    temp = T2[h];
	    T2[h] = x;
	    cout << x << " in T2[" << h << "]" << endl;
	    x = temp;
	}
	i++;
    }
    cout << "maxloop reached" << endl;
    rehash();
    insert(x);
}

void CuckooHashing() {
    cin >> len;
    cin >> maxLoop;

    T1 = new int[len];
    T2 = new int[len];

    table_clear(T1);
    table_clear(T2);

    char inst;
    int x;
    
    while(cin >> inst) {
	cin >> x;
	switch(inst) {
	  case 'i':
	    insert(x);
	    break;
	  case 'r':
	    remove(x);
	    break;
	  case 'l':
	    int temp = lookup(x);
	    if(temp == 1) {
		cout << x << " at T1[" << find_hash(x, 1) << "]" << endl;
	    } else if(temp == 2) {
		cout << x << " at T2[" << find_hash(x, 2) << "]" << endl;
	    } else if(temp == 0) {
		cout << "no " << x  << endl;
	    }
	    break;
	}
    }
}

void Heap() {
    int lines;
    cin >> lines;
    char c;
    int i, j, v, n;

    while(lines > 0) {
	cin >> c;

	switch(c){
	    case 'c':
		{
		cin >> i;
		if(i == 1) {
		    heap.setMax(true);
		    break;
		} else if(i == 2){
		    heap.setMax(false);
		    break;
		}
		}
	    case 'i':
	    	{
		cin >> i;
	    	cin >> v;
		heap.insert(i, v);
	    	break;
		}
	    case 'p':
		{
		heap.peek();
	    	break;
		}
	    case 'e':
		{
		TreeNode * n;
		n = heap.extract();
		cout << n->key << " " << n->value << endl;
	    	break;
		}
	    case 'h':
	    	{
		cin >> i;
		TreeNode* nodes = (TreeNode*) calloc(i, sizeof(TreeNode));
		j = 0;
		while(j < i) {
		    cin >> n;
		    nodes[j] = *(new TreeNode(n, 0));
		    j++;
		} 
		heap.heapify(i, nodes);
	    	break;
		}
	    case 's':
	    	{
		    cout << heap.size() << endl << endl; 
	    	break;
		}
	}
	lines--;
    }
}

void HeapSort() {
    int n, k, v;
    cin >> n;
    if(n == 1) heap.setMax(false);
    else if(n == 2) heap.setMax(true);
    cin >> n;
    TreeNode* nodes = (TreeNode*) calloc(n, sizeof(TreeNode));
    for(int i = 0; i < n; i++) {
	cin >> k;
	cin >> v;
	nodes[i] = *(new TreeNode(k, v));
    }
    heap.heapify(n, nodes);
    char c;
    cin >> c;
    if(c == 'q') {
	for(int j = 0; j < n; j++){
	    TreeNode * n;
	    n = heap.extract();
	    cout << n->key << " " << n->value << endl;
	}
	cout << endl;
    }
}


/* Main function */
int main(int argc, char** argv) {

    /* Read the option */
    int option;
    std::cin >> option;

    /* Call the respective function */
    switch(option) {

        /* Cuckoo Hashing */
        case 1:
            CuckooHashing();
        break;

        /* Binary Heap */
        case 2:
            Heap();
        break;
        
        case 3:
            HeapSort();
        break;

        /* Wrong option */
        default:
            std::cout << "Wrong option" << std::endl;

    }

    /* Close the program */
	return 0;
}

