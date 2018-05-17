#include <iostream>
#include <cstdlib>

#include "heap.h"

using namespace std;

    bool TreeNode::operator < (const TreeNode& anotherNode) {
	if(&anotherNode == NULL) return false;
	return key < anotherNode.key;
    }
    
    bool TreeNode::operator > (const TreeNode& anotherNode) {
	if(&anotherNode == NULL) return false;
	return key > anotherNode.key;
    }
    
    /*
    * If isMaxHeap == true, initialize as a MaxHeap.
    * Else, initialize as a MinHeap.
    */
    BinaryHeap::BinaryHeap(bool isMaxHea) {
    	isMaxHeap =  isMaxHea;
	root = NULL;
	elements = 0;
    }

    BinaryHeap::BinaryHeap() {
	root = NULL;
	elements = 0;
    }

    void BinaryHeap::setMax(bool isMax) {
	isMaxHeap = isMax;
    }
    
    /*
    * Given an array of TreeNode elements, create the heap.
    * Assume the heap is empty, when this is called.
    */
    void BinaryHeap::heapify(int size, TreeNode * nodes) {
    	for(int i = 0; i < size; i++){
	    if(!insert(&nodes[i])){
		cout << "failed insert" << endl;
	    }
	}
    }
    
    /*
     * Given 2 Tree Nodes, swap the positions of the nodes.
     */

    TreeNode* BinaryHeap::swap(TreeNode * parent, TreeNode * child) {
	int pk = parent->key;
	int pv = parent->value;

	int ck = child->key;
	int cv = child->value;

	parent->key = ck;
	parent->value = cv;

	child->key = pk;
	child->value = pv;
	return child;
    }

    /*
     * Reestablishes the integrety of hte heap-order property.
     */
    void BinaryHeap::orderHeap(TreeNode* current){
	if(isMaxHeap){
	    if(current->leftChild != NULL && current->key < current->leftChild->key) {
		current = swap(current, current->leftChild);
		orderHeap(root);
	    } else if(current->rightChild!=NULL&&current->key < current->rightChild->key) {
		current = swap(current, current->rightChild);
	    	orderHeap(root);
	    } else {
		if(current->leftChild != NULL) {
		    orderHeap(current->leftChild);
		}
		if(current->rightChild != NULL) {
		    orderHeap(current->rightChild);
		}
	    }
	} else {
	    if(current->leftChild != NULL && current->key > current->leftChild->key) {
		current = swap(current, current->leftChild);
		orderHeap(root);
	    } else if(current->rightChild!=NULL&&current->key > current->rightChild->key) {
		current = swap(current, current->rightChild);
	    	orderHeap(root);
	    } else {
		if(current->leftChild != NULL) {
		    orderHeap(current->leftChild);
		}
		if(current->rightChild != NULL) {
		    orderHeap(current->rightChild);
		}
	    }     
	}
    }

    bool BinaryHeap::insert(int k, int v) {
	TreeNode * node = new TreeNode(k, v);
	return insert(node);
    }

    TreeNode * BinaryHeap::findLast() {
	TreeNode * n = root;
	int i = size();
	int p = sizeof(int);
	int t = i;
	bool left;
	while(p > 0){
 	    if(t % 2){
	    	// 0
	    	if(n->leftChild != NULL){
		    n = n->leftChild;
	    	} else {
		    left = true;
		    break;
	    	}
	    } else {
	    	// 1
	    	if(n->rightChild != NULL){
	    	    n = n->rightChild;
	    	} else {
		    left = false;
		    break;
	    	}
	    }
	    t = t >> 1;
            p--;
	}
	return n;   
    }

    /*
    * insert the node into the heap.
    * return false, in case of failure.
    * return true for success.
    */
    bool BinaryHeap::insert(TreeNode * node) {
        if(root == NULL) {
	    root = node;
	    root->parent = NULL;
	    root->leftChild = NULL;
	    root->rightChild = NULL;
	    root->isLeftChild = false;
	    elements = 1;
	    return true;
	} else {
	    TreeNode * n = root;
	    int i = size();
	    int p = sizeof(int);
	    int t = i;
	    bool left;
	    while(p > 0){
		if(t % 2){
		    // 0
		    if(n->leftChild != NULL){
			n = n->leftChild;
		    } else {
			left = true;
			break;
		    }
		} else {
		    // 1
		    if(n->rightChild != NULL){
		    	n = n->rightChild;
		    } else {
			left = false;
			break;
		    }
		}
		t = t >> 1;
		p--;
	    }
	    
	    if(left) {
		node->parent = n;
		node->isLeftChild = true;
		n->leftChild = node;
	    } else {
		node->parent = n;
		node->isLeftChild = false;
		n->rightChild = node;
	    }
	    
	    elements++;
	    orderHeap(root);
	    return true;
	}
	return false;
    }
    
    /*
     * Return the minimum element of the min-heap [max element of max-heap]
     */
    TreeNode * BinaryHeap::extract() {
	TreeNode * n;
	if(size() <= 2) {
	    if(root->rightChild != NULL) {
		n = swap(root, root->rightChild);
		root->rightChild = NULL;
		elements--;
		return n;
	    } else if(root->leftChild != NULL) {
		n = swap(root, root->leftChild);
		root->leftChild = NULL;
		elements--;
		return n;
	    } else {
		n = root;
		root = NULL;
		elements = 0;
		return n;
	    }
	} else {
	    n = findLast();
	    n = swap(root, n);
	    if(n->leftChild != NULL) {
		n = swap(n, n->leftChild);
	    } else if (n->rightChild != NULL) {
		n = swap(n, n->rightChild);
	    }

	    if(n->isLeftChild) {
	    	n->parent->leftChild = NULL;
	    } else if(!n->isLeftChild) {
	    	n->parent->rightChild = NULL;
	    }
	    n->parent = NULL;
	    elements--;
	    orderHeap(root);
	    return n;
	}
    }
    
    /*
     * Return the current size of the Heap.
     */
    int BinaryHeap::size() {
        return elements;
    }
    
    void BinaryHeap::peek(){
	if(root == NULL) {
	     cout << "empty" << endl;
	     return;
	}
	cout << root->key << " " << root->value << endl;
    }

    void heapSort(TreeNode * elements, bool isReverseOrder) {
        // TODO : add your logic here.
    }
