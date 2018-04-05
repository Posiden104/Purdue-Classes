#include <iostream>
#include <string>
#include <string.h>
#include <stdlib.h>

using namespace std;

class Airport;

class route{
public:
	route* next;
	Airport* dest;
	double price;

	route(Airport* dest, double price);

};

class routeList
{
	public:
		route* first;
		route* last;

		routeList();
		void removeLastRoute();
};

class Airport
{
	public:
		string name;
		int nroutes;
		routeList routes;
		int air_number;

		// Constructor
		Airport(string s);
		Airport();
		void addRoute(route* r);


};




class Graph
{
	public:
		int n_airports;
		int n_routes;
		Airport* airports;

		int findAirport(string s, int n);

		// Constructor
		Graph();

		// Part 1: Find a ticket using Dijkstra
		void find_ticket(const string &source,const string &destination);

		// Part 2: Try to do a tour of all cities
		void eulerian_tour();

	// Add any needed private methods here
};
