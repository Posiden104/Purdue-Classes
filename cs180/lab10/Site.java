import java.util.ArrayList;

/**
 * Defines a site which has a type and a list of animals
 * 
 * @author adaskin
 * 
 */
public class Site {
    
    private SiteType type;
    private ArrayList<Animal> animals = new ArrayList<Animal>();
    
    public Site(SiteType type) {
        this.type = type;
    }
    
    public void setType(SiteType type) {
        this.type = type;
    }
    
    public SiteType getType() {
        return type;
    }
    
    // Adds an animal to the site
    public void add(Animal animal) {
        animals.add(animal);
    }
    
    // Removes an animal from the site
    public void remove(Animal animal) {
        animals.remove(animal);
    }
    
    // Returns the list of the animals in this site
    public ArrayList<Animal> getAnimals() {
        return animals;
    }
}
