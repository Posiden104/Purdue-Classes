/**
* CS180 -HW03
* Adder Program
* @author Joel Van Auken, jvanauke@purdue.edu
**/

import java.util.Scanner;

public class Adder {
    public static void main(String[] args){
	Scanner scanner = new Scanner(System.in);
	int first = scanner.nextInt();
	int second = scanner.nextInt();
	int result = first + second;
	System.out.println(result);
    }
}
