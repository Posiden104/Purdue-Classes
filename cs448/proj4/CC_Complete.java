import java.util.List;
import java.util.*;

public class CC
{

	public static int[] db;
	public static boolean debug = true;
	public static List<List<Integer>> sharedLocks;
	public static int[] exclusiveLocks;
	public static List<LogEntry> log;
	public static int time, completed;
	public static List<List<Integer>> graph;
	public static List<Transaction> transList;

	//Execute all given transactions, using locking.
	//Each element in the transactions List represents all operations performed by one transaction, in order.
	//No operation in a transaction can be executed out of order, but operations may be interleaved with other transactions if allowed by the locking protocol.
	//The index of the transaction in the list is equivalent to the transaction ID.
	//Print the log to either the console or a file at the end of the method. Return the new db state after executing the transactions.
	public static int[] executeSchedule(int[] _db, List<String> transactions)
	{
		db = _db;
		sharedLocks = new ArrayList<>();
		exclusiveLocks = new int[db.length];
		transList = new ArrayList<>();
		log = new ArrayList<>();
		int nextTrans = 0;
		int numTrans = transactions.size();
		completed = 0;
		time = -1;
		graph = new ArrayList<>();

		// read transactions
		for(int i = 1; i <= numTrans; i++){
			Transaction t = new Transaction(i, transactions.get(i-1));
			transList.add(t);
			graph.add(new ArrayList<>());
		}

		// init locks to none
		for(int i = 0; i < db.length; i++){
			sharedLocks.add(new ArrayList<>());
			exclusiveLocks[i] = -1;
		}

		boolean succ;
		// loop
		while(completed != numTrans) {
			 succ = transList.get(nextTrans).nextOp();
			if(++nextTrans == numTrans) nextTrans = 0;
		}

		// print the log
		for(LogEntry l: log){
			l.print();
		}

		return db;
	}

	/*
	    ██     ██ ██████  ██ ████████ ███████
	    ██     ██ ██   ██ ██    ██    ██
	    ██  █  ██ ██████  ██    ██    █████
	    ██ ███ ██ ██   ██ ██    ██    ██
	     ███ ███  ██   ██ ██    ██    ███████
	*/

	public static boolean write(Transaction trans, int recID, int value){
		if(requestLock(trans, LockType.exclusive, recID)){
			int oldVal = db[recID];
			db[recID] = value;
			LogEntry l = new LogEntry("W", ++time, trans.priority, recID, oldVal, value, trans.lastLogPos);
			log.add(l);
			trans.lastLogPos = time;
			return true;
		}
		return false;
	}

	/*
	    ██████  ███████  █████  ██████
	    ██   ██ ██      ██   ██ ██   ██
	    ██████  █████   ███████ ██   ██
	    ██   ██ ██      ██   ██ ██   ██
	    ██   ██ ███████ ██   ██ ██████
	*/

	public static boolean read(Transaction trans, int recID){
		if(requestLock(trans, LockType.shared, recID)){
			int value = db[recID];
			LogEntry l = new LogEntry("R", ++time, trans.priority, recID, -2, value, trans.lastLogPos);
			log.add(l);
			trans.lastLogPos = time;
			return true;
		}
		return false;
	}

	/*
	     ██████  ██████  ███    ███ ███    ███ ██ ████████
	    ██      ██    ██ ████  ████ ████  ████ ██    ██
	    ██      ██    ██ ██ ████ ██ ██ ████ ██ ██    ██
	    ██      ██    ██ ██  ██  ██ ██  ██  ██ ██    ██
	     ██████  ██████  ██      ██ ██      ██ ██    ██
	*/

	public static boolean commit(Transaction trans){
		LogEntry l = new LogEntry("C", ++time, trans.priority, -2, -2, -2, trans.lastLogPos);
		log.add(l);
		completed++;
		trans.lastLogPos = time;
		trans.completed = true;
		releaseLocks(trans);
		return true;
	}

	/*
	     █████  ██████   ██████  ██████  ████████
	    ██   ██ ██   ██ ██    ██ ██   ██    ██
	    ███████ ██████  ██    ██ ██████     ██
	    ██   ██ ██   ██ ██    ██ ██   ██    ██
	    ██   ██ ██████   ██████  ██   ██    ██
	*/

	public static boolean abort(int transID){
		Transaction trans = transList.get(--transID);
		graph.get(transID).clear();
		int prev = trans.lastLogPos;
		while(prev > 0){
			LogEntry le = log.get(prev);
			if(le.type == "W"){
				db[le.recID] = le.oldVal;
			}
			prev = le.prevTime;
		}
		LogEntry l = new LogEntry("A", ++time, trans.priority, -2, -2, -2, trans.lastLogPos);
		log.add(l);
		completed++;
		trans.lastLogPos = time;
		trans.completed = true;
		releaseLocks(trans);
		return true;
	}

	/*
	    ██       ██████   ██████ ██   ██ ███████
	    ██      ██    ██ ██      ██  ██  ██
	    ██      ██    ██ ██      █████   ███████
	    ██      ██    ██ ██      ██  ██       ██
	    ███████  ██████   ██████ ██   ██ ███████
	*/

	private static void releaseLocks(Transaction t) {
		print("released all locks for T" + t.priority);
		for(int i = 0; i < db.length; i++){
			if(exclusiveLocks[i] == t.priority){
				exclusiveLocks[i] = -1;
			}
			List<Integer> l = sharedLocks.get(i);
			l.remove(new Integer(t.priority));
		}
		return;
	}

	private static boolean requestLock(Transaction t, LockType type, int recID){
		LockType lt = getLockTypeOf(recID);
		int excLocID = exclusiveLocks[recID];
		List<Integer> shrdList = sharedLocks.get(recID);
		boolean isInShared = shrdList.contains(new Integer(t.priority));
		boolean sharedEmpty = shrdList.isEmpty();
		int ID = t.priority;
		int graphID = ID - 1;

		print("T" + ID + " requests " + type + " lock of " + recID);

		// if I have exclusive lock, I can have all the locks
		if(excLocID == ID) {
			print("granted - already has exclusive lock");
			return true;
		}

		// someone else has an exclusive lock
		if(lt == LockType.exclusive) {
			print("denied - exclusively locked");
			if(!(graph.get(graphID).contains(excLocID))) graph.get(graphID).add(excLocID);
			return false;
		}

		switch(type){
			case shared:
				if(lt != LockType.exclusive) {
					if(!isInShared) shrdList.add(new Integer(ID));
					print("granted");
					return true;
				}
				print("denied - exclusively locked");
				if(!(graph.get(graphID).contains(excLocID))) graph.get(graphID).add(excLocID);
				return false;

			case exclusive:
				if((shrdList.size() == 1 && isInShared) || sharedEmpty) {
					exclusiveLocks[recID] = ID;
					print("granted");
					return true;
				}
				print("denied - multiple shared locks");
				graph.get(graphID).addAll(shrdList);

				// remove duplicates
				Set<Integer> hs = new HashSet<>();
				hs.addAll(graph.get(graphID));
				graph.get(graphID).clear();
				graph.get(graphID).addAll(hs);

				// remove myself
				graph.get(graphID).remove(new Integer(ID));

				return false;
		}
		print("denied - defaulted");
		return false;
	}

	private static LockType getLockTypeOf(int recID){
		if(exclusiveLocks[recID] != -1){
			return LockType.exclusive;
		}
		if(!sharedLocks.get(recID).isEmpty()){
			return LockType.shared;
		}
		return LockType.none;
	}

	/*
	    ██████  ███████  █████  ██████  ██       ██████   ██████ ██   ██ ███████
	    ██   ██ ██      ██   ██ ██   ██ ██      ██    ██ ██      ██  ██  ██
	    ██   ██ █████   ███████ ██   ██ ██      ██    ██ ██      █████   ███████
	    ██   ██ ██      ██   ██ ██   ██ ██      ██    ██ ██      ██  ██       ██
	    ██████  ███████ ██   ██ ██████  ███████  ██████   ██████ ██   ██ ███████
	*/

	public static void detectDeadlocks(){
		List<Integer> res = hasCycle();
		int max = -1;
		if(!res.isEmpty()){
			print("deadlocked with " + res.size() + " transactions:");
			for(int i = 0; i < res.size(); i++){
				print("" + res.get(i));
				if(res.get(i) > max) max = res.get(i);
			}
			abort(max);
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

	/*
	    ██████  ██████  ██ ███    ██ ████████
	    ██   ██ ██   ██ ██ ████   ██    ██
	    ██████  ██████  ██ ██ ██  ██    ██
	    ██      ██   ██ ██ ██  ██ ██    ██
	    ██      ██   ██ ██ ██   ████    ██
	*/

	public static void print(String msg){
		if(debug) System.out.println(msg);
	}

}

enum LockType {shared, exclusive, none};


/*
    ██       ██████   ██████      ███████ ███    ██ ████████ ██████  ██    ██
    ██      ██    ██ ██           ██      ████   ██    ██    ██   ██  ██  ██
    ██      ██    ██ ██   ███     █████   ██ ██  ██    ██    ██████    ████
    ██      ██    ██ ██    ██     ██      ██  ██ ██    ██    ██   ██    ██
    ███████  ██████   ██████      ███████ ██   ████    ██    ██   ██    ██
*/

class LogEntry
{
	public String type;
	public int timeStamp, transID, recID, oldVal, val, prevTime;

	public LogEntry(String _type, int _timeStamp, int _transID, int _recID, int _oldVal, int _val, int _prevTime){
		type = _type;
		timeStamp = _timeStamp;
		transID = _transID;
		recID = _recID;
		oldVal = _oldVal;
		val = _val;
		prevTime = _prevTime;
	}

	public void print(){
		String log = type + ":" + timeStamp + ",T" + transID + (recID != -2 ? "," + recID : "")
									+ (oldVal != -2 ? "," + oldVal : "") + (val != -2 ? "," + val : "")
									+ "," + prevTime;
		System.out.println(log);
	}
}

/*
    ████████ ██████   █████  ███    ██ ███████  █████   ██████ ████████ ██  ██████  ███    ██
       ██    ██   ██ ██   ██ ████   ██ ██      ██   ██ ██         ██    ██ ██    ██ ████   ██
       ██    ██████  ███████ ██ ██  ██ ███████ ███████ ██         ██    ██ ██    ██ ██ ██  ██
       ██    ██   ██ ██   ██ ██  ██ ██      ██ ██   ██ ██         ██    ██ ██    ██ ██  ██ ██
       ██    ██   ██ ██   ██ ██   ████ ███████ ██   ██  ██████    ██    ██  ██████  ██   ████
*/


class Transaction
{
	public String[] operations;
	public boolean completed = false;
	public int priority, lastLogPos = -1, nextOperation = 0, numRejected = 0;
	public boolean[] eLocks = new boolean[10];
	public boolean[] sLocks = new boolean[10];

	public Transaction(int _priority, String trans){
		priority = _priority;
		operations = trans.split(";");
	}

	public boolean nextOp(){
		if(completed) return true;
		String op = operations[nextOperation];
		char first = op.charAt(0);
		int rID = 0, v = 0;
		if(first != 'C'){
			rID = Character.getNumericValue(op.charAt(2));
			if(op.charAt(3) == ','){
				v = Character.getNumericValue(op.charAt(4));
			}
		}

		boolean succ = false;

		switch(first) {
			case 'W':
				succ = CC.write(this, rID, v);
				break;
			case 'R':
				succ = CC.read(this, rID);
				break;
			case 'C':
				succ = CC.commit(this);
				break;
			default:
				return false;
		}

		if(succ) {
			nextOperation++;
			numRejected = 0;
		}
		else if(++numRejected >= 10) CC.detectDeadlocks();
		return succ;
	}

	public void print(String msg){
		if(CC.debug) System.out.println(msg);
	}
}
