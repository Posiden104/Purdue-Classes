/**
 * CS180 - Lab 03 - Person
 *
 *
 * @author jvanauke <jvanauke@purdue.edu>
 *
 * @lab 801
 *
 * @date 09/18/2014
 */

public class Person {
    
    String id;       
    int age;     
    int ldl;    
    int hdl;    
    int other; 
    
    public Person(String id, int age, int ldl, int hdl, int other) {
        this.id = id;       
        this.age = age;     
        this.ldl = ldl;    
        this.hdl = hdl;    
        this.other = other; 
    }
    
    public boolean isAdult() {
        return this.age >= 18;
    }
    
    public int getTotalCholesterol() {
        return this.ldl + this.hdl + this.other;
    }
    
    public boolean hasGoodTotalCholesterol() {
        int healthy_level;
        if (this.isAdult()) {
            healthy_level = 200;
        } else {
            healthy_level = 170;
        }
        if (this.getTotalCholesterol() < healthy_level) {
            return true;
        } else {
            return false;
        }
    }
    
    public boolean hasGoodLDL() {
        if (this.isAdult()) {
            if (this.ldl <= 130) {
                return true;
            } else {
                return false;
            }
        } else {
            if (this.ldl <= 110) {
                return true;
            } else {
                return false;
            }
        }
    }
    
    public boolean hasGoodHDL() {
        return this.hdl >= 40;
    }
    
    public void printReport() {
        System.out.println(this.id + "'s Report");
        System.out.println("Age: " + this.age + " (" + (isAdult() ? "Adult" : "Child") + ")");
        System.out.println("Total Cholesterol: " + getTotalCholesterol() + 
                           " ("+(hasGoodTotalCholesterol()?"Good":"Bad")+")");
        System.out.println("LDL: " + this.ldl + " (" + (hasGoodLDL() ? "Good" : "Bad") + ")");
        System.out.println("HDL: " + this.hdl + " (" + (hasGoodHDL() ? "Good" : "Bad") + ")");
    }
}