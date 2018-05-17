public class MigrationSimulator {
    // NOTE leave them public, because they must be accessed by WEBCAT.
     public Bird[] birds; // array of birds, TODO student case
     public Bear[] bears; // array of bears, TODO student case
    public Animal[] animals; // array of animals
    public SiteGrid sg; // two dimensional site grid
    static int niter = 5; // The number of iterations
    
    // Constructor 1
    public MigrationSimulator(int size, int nAnimal) {
        sg = new SiteGrid(size);
        animals = new Animal[nAnimal];
        initAnimals();
        sg.addAnimals(animals);
        simulateAnimals();
    }
    
    // Constructor 2
    public MigrationSimulator(int size, int nBirds, int nBears) {
    	sg = new SiteGrid(size);
    	birds = new Bird[nBirds];
    	bears = new Bear[nBears];
    	initBirds();
    	initBears();
    	sg.addAnimals(birds);
    	sg.addAnimals(bears);
    	simulateBirdsBears();
    }
    
    // initializes the array animals
    public void initAnimals() {
        for (int i = 0; i < animals.length; i++) {
            animals[i] = new Animal("ALIVE");
        }
    }
    
    // initializes the array birds
    public void initBirds() {
    	for (int i = 0; i < birds.length; i++) {
            birds[i] = new Bird("ALIVE");
        }
    }
    
    // initializes the array bears
    public void initBears() {
    	for (int i = 0; i < bears.length; i++) {
            bears[i] = new Bear("ALIVE");
        }
    }
    
    // runs the simulation for animals
    public void simulateAnimals() {
        for (int iter = 0; iter < niter; iter++) {
            for (int i = 0; i < animals.length; i++) {
                if (animals[i].getStatus().equals("ALIVE")) {
                    animals[i].makeMove(sg);
                }
            }
        }
        
        printAnimalStatistics();
    }
    
    // runs the simulation for birds and bears
    public void simulateBirdsBears() {
    	for(int iter = 0; iter < niter; iter++) {
    		for(int i = 0; i < birds.length; i++) {
    			if(birds[i].getStatus().equals("ALIVE")) {
    				birds[i].makeMove(sg);
    			}
    		}
    		for(int i = 0; i < bears.length; i++) {
    			if(bears[i].getStatus().equals("ALIVE")) {
    				bears[i].makeMove(sg);
    			}
    		}
    	}
    	printBirdBearStatistics();
    }
    
    // returns the number of animal in the given site
    public int getAnimalCount(Site site) {
        int canimal = 0;
        String status;
        for (Animal animal : site.getAnimals()) {
            status = animal.getStatus();
            if (status.equals("ALIVE")) {
                canimal++;
            }
        }
        return canimal;
        
    }
    
    // returns bird and bear counts
    public int[] getBirdBearCount(Site site) throws IllegalAnimalException {
    	int[] birdBear = new int[2];
    	for(int i = 0; i < site.getAnimals().size(); i++) {
    		if(site.getAnimals().get(i) instanceof Bird) {
    			birdBear[0]++;
    		} else if(site.getAnimals().get(i) instanceof Bear){
    			birdBear[1]++;
    		} else {
    			throw new IllegalAnimalException("An unknown animal is found in the site");
    		}
    	}
        return birdBear;
    }
    
    // prints the number of live animals
    public void printAnimalStatistics() {
        if (sg == null)
            return;
        System.out.println("The live animal count after the simulation");
        
        for (int i = 0; i < sg.xlength; i++) {
            System.out.printf("|");
            for (int j = 0; j < sg.ylength; j++) {
                System.out.printf("%3d|", getAnimalCount(sg.getSite(i,j)));
            }
            System.out.printf("\n");
        }
        
    }
    
    // prints the number of birds and bears in each site
    public void printBirdBearStatistics() {
        // For 12 birds and 30 bears, prints a cell as |12, 30|
        if (sg == null)
            return;
        System.out
            .println("\nThe live bird- and bear-counts after the simulation");
        int[] ca = new int[2];
        for (int i = 0; i < sg.xlength; i++) {
            System.out.printf("|");
            for (int j = 0; j < sg.ylength; j++) {
                try {
                    
                    ca = getBirdBearCount(sg.getSite(i,j));
                    
                } catch (IllegalAnimalException e) {
                    ca[0] = -1;
                    ca[1] = -1;
                }
				System.out.printf("%3d, %3d|", ca[0], ca[1]);
            }
            System.out.printf("\n");
        }
        
    }
    
    public static void main(String[] args) {
        
//        new MigrationSimulator(10, 1000);
         new MigrationSimulator(10, 1000, 1000); // Student case
    }
    
}
