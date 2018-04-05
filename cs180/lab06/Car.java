class Car implements Vehicle {
  // your code goes here
    
    private String color;
    private String make;
    
    public Car(String color, String make){
        this.color = color;
        this.make = make;
    }
    
    public String getColor(){
        return color;
    }
    
    public String getMake(){
        return make;
    }
}