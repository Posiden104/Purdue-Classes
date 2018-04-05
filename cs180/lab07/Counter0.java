public class Counter0 {
    public static void main(String[] args) throws InterruptedException {
        // Instantiate an object implementing Counter with inc/dec/get methods
        final Counter counter = new Counter() {
            private int value = 0;
            public void inc() {
                ++value;
            }
            public void dec() {
                --value;
            }
            public int get() {
                return value;
            }
        };
        // Instantiate an incrementing thread
        Thread inc = new Thread() {
            public void run() {
                for (int i = 0; i < 10000; i++)
                    counter.inc();
            }
        };
        // Instantiate a decrementing thread
        Thread dec = new Thread() {
            public void run() {
                for (int i = 0; i < 10000; i++)
                    counter.dec();
            }
        };
        inc.start(); // start the incrementer
        dec.start(); // start the decrementer
        inc.join();  // wait for the incremented to finish
        dec.join();  // wait for the decrementer to finish
        System.out.println(counter.get());
    }
}