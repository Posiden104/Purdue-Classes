#ifndef __HEAP_H__
#define __HEAP_H__

      
class TreeNode {
    public:
    int value;
    int key;
    TreeNode* leftChild;
    TreeNode* rightChild;
    TreeNode* parent;
    bool isLeftChild;
    
    TreeNode(int key, int value) {
        this->key = key;
        this->value = value;
        parent = NULL;
	leftChild = NULL;
        rightChild = NULL;
	isLeftChild = NULL;
    }
    
    /*
     * @Overload operator
     */
    bool operator < (const TreeNode& anotherNode);
    
    /*
     * @Overload operator
     */
    bool operator > (const TreeNode& anotherNode);
    
};


class BinaryHeap {
    private:
    bool isMaxHeap;
    TreeNode * root;
    int elements;

    public:
    /*
    * If isMaxHeap == true, initialize as a MaxHeap.
    * Else, initialize as a MinHeap.
    */
    BinaryHeap(bool isMaxHeap);
    
    BinaryHeap();

    void setMax(bool isMax);

    /*
    * Given an array of TreeNode elements, create the heap.
    * Assume the heap is empty, when this is called.
    */
    void heapify(int size, TreeNode * nodes);
    
    /*
     * Given 2 Tree Nodes, swap the positions of the nodes.
     */
    TreeNode* swap(TreeNode * parent, TreeNode * child);

    /*
     * Reestablishes the integrety of the heap-order property.
     */
    void orderHeap(TreeNode * current);

    TreeNode * findLast();

    bool insert(int key, int value);
   
   /*
    * insert the node into the heap.
    * return false, in case of failure.
    * return true for success.
    */
    bool insert(TreeNode * node);
    
    /*
     * Return the minimum element of the min-heap [max element of max-heap]
     */
    TreeNode * extract();
    
    /*
     * Return the current size of the Heap.
     */
    int size();

    void peek();
};

#endif
