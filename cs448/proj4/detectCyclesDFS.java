import java.util.ArrayList;
import java.util.*;

public class detectCyclesDFS {

    // compile with:
    // javac detectCyclesDFS.java
    // java -Xmx128M -Xms16M detectCyclesDFS


    public static ArrayList<ArrayList<Integer>> graph;
    public static int maxIters = 5;
    public static int iters = 0;

   public static void main(String[] args) {

      graph = new ArrayList<>();

      ArrayList<Integer> a1 = new ArrayList<>();
      ArrayList<Integer> a2 = new ArrayList<>();
      ArrayList<Integer> a3 = new ArrayList<>();
      ArrayList<Integer> a4 = new ArrayList<>();
      ArrayList<Integer> a5 = new ArrayList<>();
      ArrayList<Integer> a6 = new ArrayList<>();

      a1.add(2);
      a2.add(3);
      a3.add(4);
      a4.add(5);
      a4.add(6);
      a5.add(4);
      a5.add(6);
      a6.add(4);

      graph.add(a1);
      graph.add(a2);
      graph.add(a3);
      graph.add(a4);
      graph.add(a5);
      graph.add(a6);

      detectCyclesDFS.detectDeadlocks();



   }

   	public static void detectDeadlocks(){
		List<Integer> res = hasCycle();
		int max = -1;
		if(!res.isEmpty()){
			print("deadlocked with " + res.size() + " transactions:");
			for(int i = 0; i < res.size(); i++){
				print("" + res.get(i));
				max = Math.max(res.get(i), max);
			}
			print("max: " + max);
			graph.get(new Integer(max - 1)).clear();
			if(++iters < maxIters) detectDeadlocks();
		}
	}

	public static List<Integer> hasCycle() {
    List<Integer> visited = new ArrayList<>();
    for (int i = 1; i <= graph.size(); ++i) {
			List<Integer> res = hasCycle(i, visited);
      if (!res.isEmpty()) {
        return res;
      }
    }
		return new ArrayList<>();
  }

  private static List<Integer> hasCycle(int node, List<Integer> visited) {
    if (visited.contains(node)) {
    	int pos = visited.indexOf(node);
    	for(int i = 0; i < pos; i++){
    		visited.remove(0);
    	}
      return visited;
    }
    visited.add(node);
    for (Integer nextNode : graph.get(node - 1)) {
			List<Integer> res = hasCycle(nextNode, visited);
			if (!res.isEmpty()) {
        return res;
      }
    }
    visited.remove(visited.size() - 1);
    return new ArrayList<>();
  }

   public static void print(String msg){
       System.out.println(msg);
   }

   public static void printArr(ArrayList<Integer> arrlist){

      for (Integer number : arrlist) {
         System.out.println("Number = " + number);
      }
   }
}
