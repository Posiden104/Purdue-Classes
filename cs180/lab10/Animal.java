/**
 * Defines a general animal which has a site and a status
 * 
 * @author adaskin
 * 
 */
public class Animal {
	Site site = null; // animal current site
	String status = null;

	// Constructor
	public Animal(String status) {
		this.status = status;
	}

	// get method for site
	public Site getSite() {
		return site;
	}

	// mutator method for site
	public void setSite(Site site) {
		if (this.site != null)
			this.site.remove(this);

		site.add(this);
		this.site = site;
	}

	// get method for status
	public String getStatus() {
		return status;
	}

	// mutator method for status
	public void setStatus(String status) {
		this.status = status;
	}

	// kills the animal
	public void die() {
		status = "DEAD";
	}

	// makes a move in the given grid
	public void makeMove(SiteGrid sg) {
		// find a random site
		int x = (int) (sg.xlength * Math.random());
		int y = (int) (sg.ylength * Math.random());
		// move element to the new site
		setSite(sg.getSite(x, y));
	}

	@Override
	public String toString() {
		return "Animal\t" + "status=" + status;
	}

}
