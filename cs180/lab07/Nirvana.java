import java.util.Scanner;
class Nirvana extends Thread {
    public boolean enlightenment = false;
    public void run() {
        meditate();
    }
    synchronized void meditate() {
        while (!enlightenment) {
            System.out.println("Om...");
            try{
                this.wait();
            } catch(InterruptedException e) {
            }
        }
    }
    synchronized void enlighten() {
        enlightenment = true;
        this.notify();
    }
    public static void main(String[] args) throws InterruptedException {
        Nirvana t = new Nirvana();
        t.start();
        Scanner s = new Scanner(System.in);
        s.nextLine();
        t.enlighten();
        t.join();
    }
}