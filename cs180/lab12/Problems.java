import java.io.*;

public class Problems {

	public static int power(int a, int b) {
		if(b == 0) return 1;
		return ((b % 2 != 0) ? a : 1) * power(a, b / 2) * power(a, b / 2);
	}

	public static int fileCount(File file) {
		int ret = 1;
		for(File f : file.listFiles()) {
			if(f.isDirectory()) {
				ret += fileCount(f);
			} else {
				ret++;
			}
		}
		return ret;
	}

	public static void hanoi(int n, char src, char dest, char aux) {
		if(n == 0) return;
		
		if(n == 1) {
			System.out.printf("Move %d from %s to %s\n", n, src, dest);
		} else {
			hanoi(n - 1, src, aux, dest);
			hanoi(1, src, dest, aux);
			hanoi(n - 1, aux, dest, src);
		}

	}

	public static int mysterySeries(int i, int j) {
		if(i < 0 || j < 0 || i < j) return 0; 
		if(j == 0 || i == j) return 1;
		return mysterySeries(i - 1, j - 1) + mysterySeries(i - 1, j);
	}

	public static void main(String[] args) {
		System.out.printf("3^3: %d\n", power(3,3));
		System.out.printf("4^2: %d\n", power(4,2));
		
		System.out.println("---");
		
		File f = new File("/homes/jvanauke/Desktop/hello");
		System.out.println(f.getAbsolutePath());
		System.out.println(fileCount(f));

		System.out.println("---");
		
		hanoi(3,'A','C','B');

		System.out.println("---");

		System.out.printf("MS(6,2): %d\n", mysterySeries(6, 2));
		System.out.printf("MS(4,2): %d\n", mysterySeries(4, 2));
	}
}
