/**
 * Defines a two dimensional site grid: y dimension should be given. x dimension
 * is computed from the number of types defined in SiteType
 * 
 * @author adaskin
 * 
 */
public class SiteGrid {
    private Site[][] sites = null;
    int xlength;
    int ylength;
    
    public SiteGrid(int ylength) {
        this.ylength = ylength;
        this.xlength = SiteType.values().length;
        sites = new Site[xlength][ylength];// 1st row: FEEDING, 2nd row:
        // WINTERING, 3rd row: NESTING..
        initSites();
        
    }
    
    public Site getSite(int x, int y) {
        return sites[x][y];
    }
    
    // initializes the sites in the Habitat
    private void initSites() {
        SiteType type;
        SiteType[] types = SiteType.values();
        for (int i = 0; i < sites.length; i++) {
            type = types[i];
            for (int j = 0; j < sites[i].length; j++) {
                sites[i][j] = new Site(type);
            }
        }
    }
    
    // add animals randomly to the sites
    public void addAnimals(Animal[] animals) {
        int x, y;
        for (int i = 0; i < animals.length; i++) {
            x = (int) (Math.random() * sites.length);
            y = (int) (Math.random() * sites[x].length);
            animals[i].setSite(sites[x][y]);
        }
    }
    
}
