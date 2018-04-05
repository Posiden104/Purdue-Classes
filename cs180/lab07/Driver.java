public class Driver {
    public static void main(String[] args) {
        Thread t = new A();
        Runnable r = new B();
        Thread t2 = new Thread(r);
        
        t.start();
        t2.start();
    }
}