import junit.framework.TestCase;
import java.util.Arrays;

/**
 * A JUnit test case class.
 * Every method starting with the word "test" will be called when running
 * the test with JUnit.
 */
public class CarLotTests extends TestCase {
    
    /**
     * A test method.
     * (Replace "X" with a name describing the test.  You may write as
     * many "testSomething" methods in this class as you wish, and each
     * one will be called when running JUnit over this class.)
     */
    public void testCapacity() {
        CarLot c = new CarLot(5);
        assertEquals(5, c.capacity());
    }
    
    public void testNumVehicles() {
       CarLot c = new CarLot(5);
       c.add(new Car("black", "bmw"));
       assertEquals(1, c.numVehicles());
    }
    
    public void testAdd() {
        CarLot c = new CarLot(5);
        assertTrue(c.add(new Car("black", "bmw")));
    }
    
    public void testGet() {
        CarLot c = new CarLot(5);
        Car car = new Car("black", "bmw");
        c.add(car);
        assertEquals(car, c.get(0));
    }
    
    public void testRemove(){
        CarLot c = new CarLot(5);
        Car car = new Car("black", "bmw");
        c.add(car);
        assertEquals(car, c.remove(0));
    }
    
    public void testSearchByMake() {
        CarLot c = new CarLot(10);
        c.add(new Car("red", "ford"));    // 0
        c.add(new Car("blue", "gm"));     // 1
        c.add(new Car("yellow", "ford")); // 2
        c.add(new Car("red", "chevy"));   // 3
        c.add(new Car("blue", "ford"));   // 4
        c.add(new Car("orange", "gm"));   // 5
        
        boolean[] b = c.searchByMake("ford");
        assertTrue(b[0]);
        assertFalse(b[1]);
        assertTrue(b[2]);
        assertFalse(b[3]);
        assertTrue(b[4]);
        assertFalse(b[5]);
    }
    
}
