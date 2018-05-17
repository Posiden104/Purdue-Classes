
import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class Project3 {

	FuncDep funcDep;
	public static void main(String args[]) throws IOException {

		Project3 project3 = new Project3();
		project3.init();

		project3.testClosure(1);
		project3.testMinCover(2);
		project3.testKey(3);
		project3.test3NF(4);

		project3.testClosure(11);
		project3.testClosure(12);
		project3.testClosure(13);
		project3.testClosure(14);
		project3.testClosure(15);

		project3.testMinCover(21);
		project3.testMinCover(22);
		project3.testMinCover(23);
		project3.testMinCover(24);
		project3.testMinCover(25);

		project3.testKey(31);
		project3.testKey(32);
		project3.testKey(33);
		project3.testKey(34);
		project3.testKey(35);

		project3.test3NF(41);
		project3.test3NF(42);
		project3.test3NF(43);
		project3.test3NF(44);
		project3.test3NF(45);

	}

	public void init() {
		funcDep = new FuncDep();
	}

	public void testClosure(int testCaseNum) throws IOException {
		BufferedReader bufferedReader = new BufferedReader(new FileReader("Input"+testCaseNum+".txt"));
		String attrs[] = bufferedReader.readLine().split(",");
		List<String> attr = Arrays.asList(attrs);

		String fds[] = bufferedReader.readLine().split(",");
		List<String> fdList = Arrays.asList(fds);

		List<String> closure = funcDep.findClosure(attr, fdList);

		printList(closure, testCaseNum);
	}

	public void testMinCover(int testCaseNum) throws IOException {
		BufferedReader bufferedReader = new BufferedReader(new FileReader("Input"+testCaseNum+".txt"));
		String fds[] = bufferedReader.readLine().split(",");
		List<String> fdList = Arrays.asList(fds);

		List<String> minCover = funcDep.findMinCover( fdList);

		printList(minCover, testCaseNum);
	}

	public void testKey(int testCaseNum) throws IOException {
		BufferedReader bufferedReader = new BufferedReader(new FileReader("Input"+testCaseNum+".txt"));
		String attrs[] = bufferedReader.readLine().split(",");
		List<String> attr = Arrays.asList(attrs);

		String fds[] = bufferedReader.readLine().split(",");
		List<String> fdList = Arrays.asList(fds);

		List<String> key = funcDep.findKey(attr, fdList);

		printList(key, testCaseNum);
	}

	public void test3NF(int testCaseNum) throws IOException {
		BufferedReader bufferedReader = new BufferedReader(new FileReader("Input"+testCaseNum+".txt"));
		String attrs[] = bufferedReader.readLine().split(",");
		List<String> attr = Arrays.asList(attrs);

		String fds[] = bufferedReader.readLine().split(",");
		List<String> fdList = Arrays.asList(fds);

		List<List<String>> lists = funcDep.get3NFForm(attr, fdList);

		printListList(lists, testCaseNum);
	}

	public void printList(List<String> list, int testCase) {
		System.out.println("---------------------------------------------");
		System.out.println("Test case : "+testCase);
		System.out.println("---------------------------------------------");

		if (list == null){
			System.out.println("Output is null");
			System.out.println();
			System.out.println("---------------------------------------------");
			System.out.println("Test case : "+testCase + " ends");
			System.out.println("---------------------------------------------");
			return;
		}

		for (String string : list)
			System.out.print(string + ", ");
		System.out.println();
		System.out.println("---------------------------------------------");
		System.out.println("Test case : "+testCase + " ends");
		System.out.println("---------------------------------------------");
	}

	public void printListList(List<List<String>> lists, int testCase) {
		System.out.println("---------------------------------------------");
		System.out.println("Test case : "+testCase);
		System.out.println("---------------------------------------------");

		if (lists == null){
			System.out.println("Output is null");
			System.out.println();
			System.out.println("---------------------------------------------");
			System.out.println("Test case : "+testCase + " ends");
			System.out.println("---------------------------------------------");
			return;
		}
		for (List<String> list : lists){
			System.out.println();
			for (String string : list) {
				System.out.print(string + ", ");
			}
		}
		System.out.println();
		System.out.println("---------------------------------------------");
		System.out.println("Test case : "+testCase + " ends");
		System.out.println("---------------------------------------------");
	}
}
