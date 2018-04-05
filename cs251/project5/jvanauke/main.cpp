 #include <iostream>
 #include <string>
 #include "graph.h"

 using namespace std;

 // Part 1: Find tickets for an airline passenger
 void P5_part1()
 {
	 string cmd1,cmd2;
	 Graph g;

   //return;

	 cin >> cmd1;
	 while(cmd1 != "END") // Find tickets until END appears
	 {
		 cin >> cmd2;
		 g.find_ticket(cmd1, cmd2);
		 cin >> cmd1;
	 }
 }

 // Part 2: Perform an eulerian tour of the graph.
 void P5_part2()
 {
	 string cmd1;
	 Graph g;

	 cin >> cmd1;
	 g.eulerian_tour();
 }

 int main()
 {
	 int cmd;
	 cin >> cmd;
	 if(cmd == 1) P5_part1();
	 else if(cmd == 2) P5_part2();
	 return 0;
 }
