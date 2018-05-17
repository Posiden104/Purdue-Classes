#include "graph.h"
#include <limits>

using namespace std;


route::route(Airport* dest, double price){
	next = NULL;
	this->dest = dest;
	this->price = price;
}

routeList::routeList() {
	first = NULL;
	last = NULL;
}

void routeList::removeLastRoute(){
	route* r = first;
	route* l = last;
	while(r->next != l){
		r = r->next;
	}
	r->next = NULL;
	last = r;
	delete(l);
}

Airport::Airport(string s){
	name = s;
	nroutes = 0;
}

Airport::Airport(){}

void Airport::addRoute(route* r){
	if(nroutes == 0){
		routes = *new routeList();
		routes.first = r;
		routes.last = r;
	} else {
		routes.last->next = r;
		routes.last = r;
	}
	nroutes++;
}

/*
	Graph class for use in Project 5.

	This provided code does not include any specific data structure for storing
	a graph, only a list of methods that will be called from main. Based on the
	project handout and the lecture slides, select a method for storing the
	graph and implement it here.

*/

int Graph::findAirport(string s, int n){
	for(int i = 0; i < n; i++){
		if(!s.compare(airports[i].name)) return i;
	}
	return -1;
}


void printGraph(Airport* airports, int n_airports){
	for(int i = 0; i < n_airports; i++){
		cout << airports[i].name << ":" << endl;
		route* r = airports[i].routes.first;
		while(r != NULL){
			cout << "	" << r->dest->name << ": " << r->price << endl;
			r = r->next;
		}
		cout << "--------------" << endl;
	}
}



// Constructor for Graph. Read in data from the input and set up your data
// structures here.
Graph::Graph() {
	int cur = 0;
	cin >> n_airports;
	cin >> n_routes;
	airports = new Airport[n_airports];


	for(int i=0; i < n_routes; i++){

		string ssrc;
		string sdest;
		double price;
		cin >> ssrc;
		cin >> sdest;
		cin	>> price;

		int src = findAirport(ssrc, cur);
		int dest = findAirport(sdest, cur);

		if(src == -1){
			airports[cur] = *(new Airport(ssrc));
			airports[cur].air_number = cur;
			src = cur;
			cur++;
		}

		if(dest == -1){
			airports[cur] = *(new Airport(sdest));
			airports[cur].air_number = cur;
			dest = cur;
			cur++;
		}

		route* r = new route(&airports[dest], price);
		route* r2 = new route(&airports[src], price);
		airports[src].addRoute(r);
		airports[dest].addRoute(r2);

	}
	printGraph(airports, n_airports);
}

// Code for part 1. Print out the sequence of airports and price
// of the trip.
void Graph::find_ticket(const string &src, const string &dest)
{
	double cost[n_airports];
	bool visited[n_airports];
	int n_left = n_airports;
	routeList path = *(new routeList());

	for(int i = 0; i < n_airports; i++){
		cost[i] = std::numeric_limits<float>::max();
		visited[i] = false;
	}

	int start = findAirport(src, n_airports);
	Airport a = airports[start];
	visited[start] = true;
	cost[start] = 0;
	route* r = new route(&a, 0.0);
	path.first = r;
	path.last = r;

	while(n_left > 0){
		visited[a.air_number] = true;
		n_left--;
		double low = std::numeric_limits<float>::max();
		//string nextAirport = "";
		route* cr = a.routes.first;
		while(cr != NULL){
			int cst = cost[a.air_number] + cr->price;
			if(cst < cost[cr->dest->air_number]){
				cost[cr->dest->air_number] = cst;
			}
			if (cst < low) {
				low = cst;
			}
			cr = cr->next;
		}

		int nxtapt = 0;
		double curCost = std::numeric_limits<float>::max();
		for(int i = 0; i < n_airports; i++){
			if(!visited[i] && cost[i] < curCost){
				curCost = cost[i];
				nxtapt = i;
			}
		}
		a = airports[nxtapt];
		
		route* nr = new route(&a, low);
		path.last->next = nr;

		if(!a.name.compare(dest)){
			n_left = 0;
			route* n = path.first;
			while(n != NULL){
				cout << n->dest->name << " ";
				n = n->next;
			}
			cout << path.last->price << endl;
		}
	}
	// search for the index of cost that is lowest & has not been visited.
	// Make that airport the new a. Add that airport to path.
}

// Code for part 2. Print out the tour sequence or "not possible"
// if not all cities can be visited.
void Graph::eulerian_tour()
{

}
