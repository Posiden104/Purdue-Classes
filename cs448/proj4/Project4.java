import java.util.Arrays;
import java.util.List;

public class Project4
{

	public static int[] newDB()
	{
		return new int[]{0,1,2,3,4,5,6,7,8,9};
	}

	public static void main(String[] args)
	{
		//Test Cases
		//1) Handout test case
		List<String> tc1 = Arrays.asList(new String[]{"W(1,5);R(2);W(2,3);R(1);C","R(1);W(1,2);C"});
		//2) Three transactions
		List<String> tc2 = Arrays.asList(new String[]{"R(6);W(7,2);W(5,8);W(6,2);C","R(4);W(2,4);R(5);W(7,3);C","R(9);R(6);W(1,9);C"});
		//3) Deadlocks
		List<String> tc3 = Arrays.asList(new String[]{"W(4,1);R(1);W(1,2);C","W(5,2);R(1);R(2);W(2,3);C","W(6,3);R(2);W(2,4);C"});
		//4) No conflicts
		List<String> tc4 = Arrays.asList(new String[]{"R(1);R(2);R(3);W(4,1);C","R(1);R(2);R(3);W(5,2);C"});
		//5) Multiple Deadlocks
		List<String> tc5 = Arrays.asList(new String[]{"R(1);W(2,1);C","R(2);R(3);W(1,2);C","R(1);W(3,3);C"});
		//6)Larger Deadlocks cycle
		List<String> tc6 = Arrays.asList(new String[]{"R(1);W(2,1);C","R(2);W(3,2);C","R(3);W(4,3);C","R(4);W(1,4);C"});
		//Consult the ExpectedOutputs.txt for the expected DB state and log

		System.out.println("Handout Test Case:\n\t" + Arrays.toString(CC.executeSchedule(newDB(),tc1)) + "\n");
		System.out.println("Three Transactions Test Case:\n\t" + Arrays.toString(CC.executeSchedule(newDB(),tc2)) + "\n");
		System.out.println("Deadlocks Test Case:\n\t" + Arrays.toString(CC.executeSchedule(newDB(),tc3)) + "\n");
		System.out.println("No Conflicts Test Case:\n\t" + Arrays.toString(CC.executeSchedule(newDB(),tc4)) + "\n");
		System.out.println("Multiple Deadlocks Test Case:\n\t" + Arrays.toString(CC.executeSchedule(newDB(),tc5)) + "\n");
		System.out.println("Larger Deadlock Cycle Test Case:\n\t" + Arrays.toString(CC.executeSchedule(newDB(),tc6)) + "\n");


	}
}
