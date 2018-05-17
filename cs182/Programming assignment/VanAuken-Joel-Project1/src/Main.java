

import java.util.ArrayList;
import java.util.Scanner;


/* 
 * @author Joel Van Auken
 * 
 */

public class Main {
	
	/*
	 * GENERAL PROCESS
	 * 
	 * 2d array
	 * each row (i.e. x in [x][y]) holds each clause
	 * each element in the row (i.e. y in [x][y]) holds 1 boolean
	 * 
	 */
	
	public static boolean[] vars;
	public static int[][] clauses;
	public static ArrayList<String> arg;
	public static Main m;
	
	/*************************
	 *   Recursion Method 	 *
	 * 		The "GUTS" 		 *
	 * Input:integer current *
	 *    Output: Boolean	 *
	 *************************/

	public boolean iterate(int current) {
		if(current > vars.length) System.exit(0);
		// Set myself to true
		vars[current -1] = true;
		if(current - 1 < vars.length - 1){ // Is there someone after me?
			if(iterate(current + 1)){ // Keep the chain moving
				return true;
			}
		} else { // I'm the last one!!
			if(evaluate()) { // Is this set of conditions satisfied? 
				return true;
			} else { // I need to be false
				vars[current - 1] = false;
				return evaluate();
			}
		}
		
		if(vars[current - 1]){ // I'm true and its my turn to turn false
			vars[current - 1] = false;
			
			if(current - 1 < vars.length -1){ // Is there someone after me?
				if(iterate(current + 1)){ // Keep the chain moving
					return true;
				} else {
					return false;
				}
			}
		} 
		// I'm false, and everything after me is wrong
		return false;
	}
	
	/**************************
	 *   Evaluation Method    *
	 *                        *
	 *   Input: NULL	      *    
	 *   Uses int[][] clause  *
	 *   Output: Boolean      *
	 **************************/
	public static boolean evaluate() {
		int currentVar;
		//Iterate through array
		for(int i = 0; i < clauses.length; i++){
			//This loop check the "AND" between clauses
			//(if all clauses have truth, it will return true)
			
			for(int j = 0; j < clauses[i].length; j++){  //This checks for the "OR" of each clause
				currentVar = clauses[i][j];
				
				// The currentVar - 1 puts the currentVar in terms of the array
				
				// Check if the selected variable is true
				if(currentVar < 0) { // value is negated in clause
					if(!vars[(currentVar * -1) - 1]){
						// This clause is true [SATASFIED]
						break;
					}
				} else if(vars[currentVar - 1]) { // value is normal in clause
					// This clause is true [SATASFIED]
					break;
				}
				
				if(j == clauses[i].length -1) {
					// This clause has no truth to it.
					return false;
				}
			}
		}
		//All clauses had a true in them
		return true;
	}
	
	public static void printCorrect(boolean [] sols) {
		System.out.println("Satisfiable");
		for(int i = 0; i < sols.length; i ++) {
			System.out.print(sols[i] + " ");
		}
		System.out.println();
	}
	
	public static void parseInput(ArrayList<String> input){
		String test = "";
		int num_vars = 0;
		int num_clauses = 0;
		
		// Iterates through the input string
		for(int i = 0; i < input.size(); i ++) { 
			
			// Triggers on a new test
			if(input.get(i).contains("Test")){ 
				test = input.get(i);
				i++;
				
				// Splits the variables clauses line
				String[] args = input.get(i).split(" "); 
				 
				//Grab variables and clauses 
				num_vars = Integer.parseInt(args[0]); 
				num_clauses = Integer.parseInt(args[1]); 
				
				// Initialization of vars and clauses
				vars = new boolean[num_vars];
				clauses = new int[num_clauses][];
				i++;
				
				System.out.println(test + ": " + num_vars + " Variable(s) " + num_clauses + " Clause(s)");

				int cnc = 0; // Current number of clauses
				
				boolean flag = false; // Flag for while loop
				
				while(!flag && !input.get(i).contains("Test") && cnc < num_clauses) {
					//parse the clauses
					args = input.get(i).split(" ");  						// Splits current line into "words"
					clauses[cnc] = new int[args.length]; 					// Initializes the array of conditions at the current clause index
					
					for(int j = 0; j < args.length; j++) {					// Iterates through the "words" of the clause
						clauses[cnc][j] = Integer.parseInt(args[j]); 		// Assigns the current "word" to the correct index
					}
					
					// Move on to the next clause
					cnc++;
					i++;
					
					// Is the index at the end of the input?
					if(i >= input.size()) {
						flag = true;
					}
				}
				
				
				// All clauses are entered, calculate results
				if(m.iterate(1)){
					printCorrect(vars);
				} else {
					System.out.println("Unsatisfiable");
				}
				
				// Subtract i for the increase at the bottom of the for loop
				i--;
			}
			// i increased
		}
	}
	
	public static void main(String[] args) {
		m = new Main();
		arg = new ArrayList<String>();
		Scanner sc = new Scanner(System.in);
		
		try {
			while(sc.hasNextLine()) {
				String tmp = sc.nextLine();
				if(!tmp.equals("EOF")){ // || !tmp equals(EOF)) {
					arg.add(tmp);
				} else {
					sc.close();
				}
			}		
			sc.close();
		} catch (Exception e) {}
		
		parseInput(arg);		
		
	}
}
