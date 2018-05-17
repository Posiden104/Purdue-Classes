
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Collection;
import java.util.*;


public class FuncDep {

	private boolean debug = true;

	/*
		Helper, prints lists
	 */

	public void printList(String name, List<String> list){
		if(debug) {
			System.out.println(name + ": ");
			for (String s : list) {
				System.out.println(s);
			}
			System.out.println();
		}
	}

	public void printArray(String name, String[] list){
		if(debug) {
			System.out.println(name + ": ");
			for (String s : list) {
				System.out.println(s);
			}
			System.out.println();
		}
	}

	public void print(String msg) {
		if(debug) System.out.println(msg + '\n');
	}


	public List<String> findMinCover(List<String> fds, boolean union) {
		ArrayList<String> f = new ArrayList<>(findMinCover(fds));
		if(union){
				// Union
				ArrayList<String> FDsToRemove = new ArrayList<String>();
				ArrayList<String> FDsToAdd = new ArrayList<String>();
				boolean unionized = false;
				for(String s: f){
					String lhs = s.split("->")[0];
					String rhs = s.split("->")[1];
					String toAdd = s;
					unionized = false;
					for(String s2: f){
						if(FDsToRemove.contains(s2)) continue;
						if(!s.equals(s2)){
							String lhs2 = s2.split("->")[0];
							if(lhs.equals(lhs2)){
								String rhs2 = s2.split("->")[1];
								toAdd = toAdd + rhs2;
								unionized = true;
								FDsToRemove.add(s2);
							}
						}
					}
					if(unionized) {
						FDsToRemove.add(s);
						FDsToAdd.add(toAdd);
					}
				}

				f.removeAll(FDsToRemove);
				f.addAll(FDsToAdd);
		}
		return f;
	}


	/**
	 * Find the closure of attribute set attr under functional dependencies fds.
	 * Refer to Algorithm 16.1 (15.1 in edition 7) for more details
	 *
	 * @param attr, list of attributes. Each attribute is an uppercase letter "A", "B", etc
	 * @param fds, list of functional dependencies of format "AB->XY"
	 * @return closure of attributes attr under fds
	 */
	public List<String> findClosure(List<String> attr, List<String> fds) {
		boolean debugsave = debug;
		debug = false;

		printList("attr", attr);
		printList("fds", fds);

		ArrayList<String> xp = new ArrayList<>(attr);
		ArrayList<String> oldxp = new ArrayList<>();

		do{
			for(String fd : fds) {
				printList("oldxp", oldxp);
				printList("xp", xp);
				oldxp = new ArrayList<>(xp);
				String[] fdsplit = fd.split("->");
				ArrayList<String> fdl = new ArrayList<>(Arrays.asList(fdsplit[0].split("(?!^)")));
				ArrayList<String> fdr = new ArrayList<>(Arrays.asList(fdsplit[1].split("(?!^)")));

				printList("fd left", fdl);
				printList("fd right", fdr);

				boolean all = true;
				for(String s: fdl){
					if(!oldxp.contains(s)){
						print("old xp does not contain" + s);
						all = false;
						break;
					}
				}

				if(all){
					for(String s: fdr){
						if(!xp.contains(s)){
							print("adding " + s + " to xp");
							xp.add(s);
						} else {
							print(s + " is already in xp");
						}
					}
				}

/*
				if(oldxp.containsAll(fdl)){
					printList("need to add", fdr);
					for(String s: fdr) {
						if(!xp.contains(s)){
							print("Adding " + s + " to xp");
							xp.add(s);
						} else {
							print(s + " is already in xp");
						}
					}
				} else {
					printList("oldxp", oldxp);
					printList("oldxp does not contain ", fdl);
				}
				*/
			}
			printList("new xp", xp);
		} while(!xp.equals(oldxp));

		debug = debugsave;
		return xp;
	}

	/**
	 * Find the minimum cover of a set of functional dependencies fds
	 * Refer to Algorithm 16.2 (15.2 in edition 7)
	 *
	 * @param fds, list of functional dependencies of format "AB->XY"
	 * @return minimum cover over set fds
	 */
	public List<String> findMinCover(List<String> fds) {
		boolean debugsave = debug;
		debug = false;

		ArrayList<String> f = new ArrayList<>();

		// decomp
		for(String s: fds) {
			String[] s_split = s.split("->");
			String lhs = s_split[0];
			String[] rhs = s_split[1].split("(?!^)");
			for(String s2: rhs) {
				f.add(lhs + "->" + s2);
			}
		}

		// extraneous Attr
		printList("before extraneous removal", f);

		ArrayList<String> FDsToRemove = new ArrayList<String>();
		ArrayList<String> FDsToAdd = new ArrayList<String>();

		for(String s: f){
			String[] s_split = s.split("->");
			String[] lhs = s_split[0].split("(?!^)");

			if(lhs.length == 1) continue;

			String rhs = s_split[1];
			ArrayList<String> fp = new ArrayList<>(f);

			for(String b: lhs) {
				String lhsMinusB = "";
				for(String b2: lhs) {
					if(!b.equals(b2)){
						lhsMinusB += b2;
					}
				}

				// check if closure still contains rhs
				ArrayList<String> attr = new ArrayList<String>();
				attr.add(lhsMinusB);
				List<String> closure = findClosure(attr, f);
				if(closure.contains(rhs)){
					String XminusA = lhsMinusB + "->" + rhs;
					FDsToRemove.add(s);
					FDsToAdd.add(XminusA);
				}
			}
		}

		f.removeAll(FDsToRemove);
		f.addAll(FDsToAdd);

		printList("after extraneous removal", f);

		// redundant FD
		printList("before redundant removal", f);
		ArrayList<String> redundantFDs = new ArrayList<String>();

		for(String s: f) {
				String[] s_split = s.split("->");
				String[] t_arr = s_split[0].split("(?!^)");
				String b = s_split[1];

				ArrayList<String> t = new ArrayList<>(Arrays.asList(t_arr));

				ArrayList<String> fp = new ArrayList<>(f);
				fp.remove(s);
				fp.removeAll(redundantFDs);
				boolean flag = true;

				print("checking if " + s + " is redundant");
				while(flag) {
					flag = false;
					for(String x : fp){
						String[] x_split = x.split("->");
						ArrayList<String> x_ind = new ArrayList<String>(Arrays.asList(x_split[0].split("(?!^)")));
						String y = x_split[1];
						printList("t looking for " + b, t);
						if(t.containsAll(x_ind) && !t.contains(y)){
								flag = true;
								t.add(y);
						}
					}
				}
				if(t.contains(b)){
					print("found " + b);
					redundantFDs.add(s);
				}
		}

		for(String s: redundantFDs){
			int first = f.indexOf(s);
			int last = f.lastIndexOf(s);
			if(first == last) {
				f.remove(s);
				continue;
			}
			while(first != last) {
				f.remove(last);
				first = f.indexOf(s);
				last = f.lastIndexOf(s);
			}
		}
		printList("after redundant removal", f);

		// Dont Union

		debug = debugsave;
		return f;
	}

		/**
		 * Find the key of relation R defined by set of attribute set attr.
		 * Refer to Algorithm 16.2(a) (15.2(a) in edition 7) for more details
		 *
		 * @param attr, complete list of attributes in relation R. Each attribute is an uppercase letter "A", "B", etc
		 * @param fds, list of functional dependencies of format "AB->XY"
		 * @return list of attributes that defines the key of relation R
		 */
		public List<String> findKey(List<String> attr, List<String> fds) {
			boolean debugsave = debug;
			debug = false;

			ArrayList<String> k = new ArrayList<>(attr);

			printList("before algor", k);

			for(String a: attr){
					print("removing " + a);
					k.remove(a);
					List<String> closure = findClosure(k, fds);
					printList("closure", closure);
					if(!closure.containsAll(attr)){
						print("adding " + a + " back");
						k.add(a);
					} else {
						print("Dont need to add " + a + " back");
					}
					printList("k", k);
			}

			printList("after algor", k);

			debug = debugsave;
			return k;
		}

	/**
	 * Synthesize the relation into 3NF with Dependency Preservation and Nonadditive Join Property
	 * Refer to Algorithm 16.6 (15.4 in edition 7) for more details
	 *
	 * @param attr, complete list of attributes in relation R. Each attribute is an uppercase letter "A", "B", etc
	 * @param fds, list of functional dependencies of format "AB->XY"
	 * @return List of relations, where each relation is a list of attributes.
	 */
	public List<List<String>> get3NFForm(List<String> attr, List<String> fds) {
		boolean debugsave = debug;
		debug = false;

		ArrayList<String> g = new ArrayList<>(findMinCover(fds, true));
		List<List<String>> ret = new ArrayList<>();
		List<String> key = new ArrayList<>(findKey(attr, fds));
		boolean foundKey = false;

		printList("min cover", g);
		printList("key", key);

		for(String s: g){
			print("fd: " + s);
			String[] s_split = s.split("->");
			String[] rhs = s_split[1].split("(?!^)");
			String[] lhs = s_split[0].split("(?!^)");

			ArrayList<String> d = new ArrayList<>();
			for(String s2: rhs){
				d.add(s2);
			}
			for(String s2: lhs){
				d.add(s2);
			}

			printList("d", d);

			if(d.containsAll(key)) {
				print("found key");
				foundKey = true;
			}
			ret.add(d);
		}

		if(!foundKey){
			ret.add(key);
		}

		List<List<String>> ret_copy = new ArrayList<>(ret);

		for(List<String> s: ret_copy){
			int toRemove = -1;
			if(s.isEmpty()){
				print("found empty");
				toRemove = ret.indexOf(s);
			} else {
				printList("look for projections of", s);
				for(List<String> s2: ret){
					if(!s.equals(s2)){
						if(s.containsAll(s2)){
							toRemove = ret.indexOf(s2);
						}
					}
				}
			}
			if(toRemove > -1) ret.remove(toRemove);
		}

		debug = debugsave;
		return ret;
	}

}
