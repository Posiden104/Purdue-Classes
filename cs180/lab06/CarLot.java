import java.lang.ArrayIndexOutOfBoundsException;
import java.lang.IndexOutOfBoundsException;

class CarLot implements Inventory<Car> {
    // your code goes here
    
    private Car[] cars;
    private int capacity;
    
    public CarLot(int cap) {
        capacity = cap;
        cars = new Car[cap];
    }
    
    public int capacity() {
        return capacity;
    }
    
    public int numVehicles() {
        int num = 0;
        for(int i = 0; i < cars.length; i++) {  
            if(cars[i] != null){
                num++;
            }
        }
        
        return num;
    }
    
    public boolean add(Car vehicle) {
        for(int i  = 0; i < cars.length; i++) {
            if(cars[i] == null) {
                cars[i] = vehicle;
                return true;
            }
        }
        return false;
    }
    
    public Car get(int location) {
        try{
            return cars[location];
        } catch (IndexOutOfBoundsException e) {
            return null;
        } catch (NullPointerException e) {
            return null;
        }
    }
    
    public Car remove(int location){
        Car ret = cars[location];
        cars[location] = null;
        return ret;
    }
    
    public boolean[] searchByMake(String make) {
        boolean[] ret = new boolean[cars.length];
        for(int i = 0; i < ret.length; i++) {
            if(cars[i] != null && cars[i].getMake().equals(make)) {
                ret[i] = true;
            } else {
                ret[i] = false;
            }
        }    
        return ret;
    }
}