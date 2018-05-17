import java.util.Scanner;
/**
 * CS 180 - Lab 04 - Repetition
 * 
 * 
 * 
 * @author Joel Van Auken <jvanauke@purdue.edu>
 * 
 * @lab 801
 *
 * @date sept 25, 2014
 */

public class Repetition {
    
    public static void main(String[] args) {
        even();
        System.out.println();
        powers();
        System.out.println();
        alphabet();
        System.out.println();
        vertical("Hello");
        System.out.println();
        testResults();
        System.out.println();
    }
    
    public static void even() {
        for (int i = 0; i <= 100; i += 2) {
            System.out.print(i);
        }
    }
    
    public static void powers() {
        for (int i = 0; Math.pow(2, i) < 1000; i++) {
            System.out.print(Math.pow(2, i));
        }
    }
    
    public static void alphabet(){ 
        String aph = "";
        for (char i = 97; i < 123; i++) {
            aph = aph + i;
        }
        System.out.print(aph);
    }
    
    public static void vertical(String s) {
        for (int i = 0; i < s.length(); i++) {
            System.out.println(s.charAt(i));
        }
    }
    
    public static void testResults() {
        int sum = 0;
        int number = 0;
        int low = 1000000;
        int high = 0;
        
        Scanner s = new Scanner(System.in);
        System.out.println("Enter scores now:");
        while(s.hasNextInt()) {
            int next = s.nextInt();
            sum += next;
            if (next < low) low = next;
            if (next > high) high = next;
            number++;
        }
        
        double avg = sum / number;
        avg = Math.floor(avg);
        
        System.out.println("Lowest Score:  " + low);
        System.out.println("Highest Score: " + high);
        System.out.println("Average Score: " + (int) avg);
    }
}