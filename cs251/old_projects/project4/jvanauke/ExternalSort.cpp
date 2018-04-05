#include <fstream>
#include <ctime>
#include "ExternalSort.h"


/*
	External Sorting algorithm.
	Step 1: Reads the input file one chunk at a time
	Step 2: Sorts each chunk
	Step 3: Writes each chunk to a temporary file
	Step 4: Performs a k-wise merge on the temporary files

*/

int currentChunk;
string chunkLoc;
ofstream outf;
ifstream inf;
unsigned long long numItems;
int csize;

string getChunkFile(int cchunk){
	cchunk++;
	string ret("");
	ret.append("./");
	ret.append(chunkLoc);
	ret.append("chunk");

	if(currentChunk >= 10000){
	} else if(cchunk >= 1000){
		ret.append("0");
	} else if(cchunk >= 100) {
		ret.append("00");
	} else if(cchunk >= 10) {
		ret.append("000");
	} else {
		ret.append("0000");
	}	
	ret.append(to_string(cchunk));
	ret.append(".txt");
	return ret;
}

void extern_sort(string in_fname, string out_fname, string chunk_location, int chunk_size) {
	clock_t start;
	double diff;

	inf.open(in_fname);

	chunkLoc = chunk_location;
	csize = chunk_size;
	currentChunk = 0;

	bool stop = false;
	
	start = clock();

	while(inf.good()) {
		string carray[chunk_size] = {};
		for(int i = 0; i < chunk_size; i++){
			if(!inf.good()){
				stop = true;
				break;
			}
			string tmp;
			getline(inf, tmp);
			if(!tmp.empty()) {
				numItems++;
				carray[i] = tmp;
			}
		}
		if(stop) break;

		sort(carray, 0, (csize - 1), csize);
		
		string file = getChunkFile(currentChunk);

		fstream chunkf;
		chunkf.open(file, fstream::out);
			if(chunkf.is_open()){
				cout << file << endl;
				for(int i = 0; i < chunk_size; i++){
					chunkf << string(carray[i]) << endl;
				}
			}
		chunkf.close();
		
		currentChunk++;
	}
	diff = (clock() - start) / (double)CLOCKS_PER_SEC;
	cout << "Read time: " << diff << endl;

	start = clock();

	k_merge(chunk_location, out_fname, currentChunk);
	
	diff = (clock() - start) / (double)CLOCKS_PER_SEC;
	cout << "Sort time: " << diff << endl;

	inf.close();
	return;
} 

/*
	Your favorite n*log(n) sorting algorithm.
	Be sure that you use an in-place version of the algorithm.
	i.e. It does not generate a new array to return, but rather
	works by swapping elements in the input array.
	
	Possible options:
		-Quicksort
		-Mergesort
		-Heapsort
		
*/

void swp(int i, int j, string* array) {
	string tmp = array[i];
	array[i] = array[j];
	array[j] = tmp;
	return;
}

int partition(string* array, int left, int right){
	int mid = left + (right-left) /2;
	string p = array[mid];

	swp(mid, left, array);

	int i = left + 1;
	int j = right;
	
	while(i <= j){
		while(i <= j && array[i].compare(p) <= 0){
			i++;
		}
		while(i <= j && array[j].compare(p) > 0) {
			j--;
		}
		if(i < j){
			swp(i, j, array);
		}
	}
	swp((i-1), left, array);
	return i -1;
}

void sort(string* array, int left, int right, int size) {
	//Quicksort
	if(left >= right) return;
	
	int part = partition(array, left, right);
	sort(array, left, part - 1, csize);
	sort(array, part + 1, right, csize);
	return;
}

int findMinStr(string* array, int len){
	string min = array[0];
	int i = 0;
	if(min.empty()){
		while(min.empty()){
			i++;
			min = array[i];
		}
	}
	int ret = i;
	for(i; i < len; i++){
		if(!array[i].empty() && min.compare(array[i]) > 0){
			min = array[i];
			ret = i;
		}
	}
	return ret;
}

// Merges the k temporary files
void k_merge(string chunk_location, string out_fname, int num_chunks)
{
	outf.open(out_fname);

	fstream chunks[num_chunks] = {};
	string lines[num_chunks] = {};

	for(int i = 0; i < num_chunks; i++){
		string file = getChunkFile(i);
		string tmp;
		chunks[i].open(file, fstream::in);
		getline(chunks[i], tmp);
		lines[i] = tmp;
	}
	
	for(int i = 0; i < numItems; i++){
		int min = findMinStr(lines, num_chunks);
		outf << lines[min] << endl;
		string tmp;
		getline(chunks[min], tmp);
		lines[min] = tmp;
	}
	
	for(int i = 0; i < num_chunks; i++){
		chunks[i].close();
	}
	
	outf.close();

	return;
}
