import javax.swing.JOptionPane;

public class CollegeFeeCalculator {
	/**
	 * CS180 - Lab 08 - CollegeFeeCalculator
	 *
	 * Calculates the cost of college
	 *
	 * @author Joel Van Auken <javanauke@purdue.edu>
	 *
	 * @lab 801
	 *
	 * @date 11/6/2-14
	 */

	public String name;
	public boolean fullTime;
	public int creditHrs = -1;
	public boolean repeat;
	public String residence;
	public boolean onCampus;
	public String hall;
	public int houseExpense = 0;
	public int tuition;
	public int total;

	String[] enrollment = { "Part-time", "Full-time" };
	String[] residency = { "In-state", "Out-of-state", "International" };
	String[] housing = { "ON-Campus", "OFF-Campus" };
	String[] halls = { "Earhart", "Hillenbrand", "Owen", "Windsor" };

	public CollegeFeeCalculator() {
		JOptionPane.showMessageDialog(null, "Welcome to CollegeFeeCalculator!", "CollegeFeeCalcualtor", JOptionPane.INFORMATION_MESSAGE);

		do {
			repeat = false;
			name = JOptionPane.showInputDialog(null, "Please enter your name, them press OK", "Name", JOptionPane.QUESTION_MESSAGE);
			if (name == null)
				break;
			int i = JOptionPane.showOptionDialog(null, "Please select your enrollment", "Enrollment", JOptionPane.PLAIN_MESSAGE, JOptionPane.QUESTION_MESSAGE, null, enrollment, null);
			if (i == -1)
				break;
			else if (i == 1)
				fullTime = true;
			else
				fullTime = false;
			do {
				if (repeat)
					JOptionPane.showMessageDialog(null, "Please enter valid credit hours for the current enrollment", "Invalid no. of credits", JOptionPane.ERROR_MESSAGE);
				try{
					creditHrs = Integer.parseInt(JOptionPane.showInputDialog(null, "Please enter the no. of credit hours, then press OK", "Credit Hours", JOptionPane.QUESTION_MESSAGE));
					if (creditHrs == -1)
						break;
					repeat = true;
				} catch (NumberFormatException e) {
					break;
				}
			} while (fullTime ? (creditHrs < 8) : (creditHrs) >= 8);
			if(creditHrs == -1) break;

			residence = (String) JOptionPane.showInputDialog(null, "Please select the appropriate residency", "Residency", JOptionPane.QUESTION_MESSAGE, null, residency, null);
			if(residence == null) break;
			switch (residence) {
			case "In-state":
				tuition = fullTime ? 4996 : (350 * creditHrs);
				break;
			case "Out-of-state":
				tuition = fullTime ? (9401 + 4996) : ((600 + 350) * creditHrs);
				break;
			case "International":
				tuition = fullTime ? (1000 + 9401 + 4996) : ((70 + 600 + 350) * creditHrs);
				break;
			}

			String p =(String) JOptionPane.showInputDialog(null, "Please select your housing", "Housing", JOptionPane.QUESTION_MESSAGE, null, housing, null);
			if(p == null) break;
			onCampus = p.equals("ON-Campus") ? true : false;
			if (onCampus) {
				switch ((String) JOptionPane.showInputDialog(null, "Please select the residence hall", "Residence-Hall", JOptionPane.QUESTION_MESSAGE, null, halls, null)) {
				case "Earhart":
					houseExpense = 4745;
					break;
				case "Hillenbrand":
					houseExpense = 5307;
					break;
				case "Owen":
					houseExpense = 4130;
					break;
				case "Windsor":
					houseExpense = 4150;
					break;
				}
			}

			total = tuition + houseExpense;
			String results;
			results = String.format("Name: %s\nCredit Hours: %d\nEnrollment: %s\nResidency: %s\nTuition fee: $%s\nHousing Expense: $%s\nTotal Sem. Fee: $%s", name, creditHrs, (fullTime ? "Full-time" : "Part-time"), residence, tuition, houseExpense, total);
			JOptionPane.showMessageDialog(null, results, "CollegeFeeCalcualtor", JOptionPane.INFORMATION_MESSAGE);

		} while (!(JOptionPane.showConfirmDialog(null, "Would you like to preform another fee calculation?", "Are you done?", JOptionPane.YES_NO_OPTION) == 1));
	}

	public static void main(String[] args) {
		CollegeFeeCalculator c = new CollegeFeeCalculator();
	}
}
