import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;

public class Stats {
	public static HashMap<String, Integer> wins() throws IOException {
		HashMap<String, Integer> ret = new HashMap<String, Integer>();
		BufferedReader br = new BufferedReader(new FileReader("InputFile.txt"));
		String s;
		String[] names;
		String[] winners;
		boolean first;

		while ((s = br.readLine()) != null) {
			if (s.startsWith("1")) {
				first = true;
			} else {
				first = false;
			}
			s = s.substring(2);
			names = s.split(" vs. ");
			winners = names[first ? 0 : 1].split(" ");
			
			for(int i = 0; i < winners.length; i++ ) {
				if(ret.get(winners[i]) == null) {
					ret.put(winners[i], 1);
				} else {
					int old = ret.get(winners[i]);
					ret.remove(winners[i]);
					ret.put(winners[i], ++old);
				}
			}
		}
		br.close();
		return ret;
	}
	
	public static String winner(HashMap<String, Integer> hm) {
		int win = 0;
		String winner = "Noone won";
		Iterator<Entry<String, Integer>> it = hm.entrySet().iterator();
		while(it.hasNext()) {
			Map.Entry<String, Integer> player = (Map.Entry<String, Integer>)it.next();
//			System.out.println(player.getKey() + " > " + player.getValue());
			if(player.getValue() > win) {
				winner = player.getKey();
				win = player.getValue();
			}
		}
		
//		System.out.println(hm.get(winner));
		return winner;
	}
	
	public static void main(String[] args) throws IOException {
		System.out.println(winner(wins()));
	}
}