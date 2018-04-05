interface Inventory<V extends Vehicle> {
    /**
     * @return the storage capacity of the inventory
     */
    int capacity();
    
    /**
     * @return the number of vehicles in the inventory
     */
    int numVehicles();
    
    /**
     * add a vehicle to the inventory
     * @param vehicle to be added
     * @return true if there is space for the vehicle, false otherwise
     */
    boolean add(V vehicle);
    
    /**
     * return the vehicle at the specified location
     * @param location from which to retrieve the vehicle
     * @return vehicle at that location, or null if the location is empty
     */
    V get(int location);
    
    /**
     * remove and return the vehicle at the specified location
     * @param location of the vehicle to remove
     * @return vehicle removed from that location, or null if the location is empty
     */
    V remove(int location);
    
    /**
     * search for vehicles that match the specified make
     * @param make of the vehicles to search for
     * @return array of boolean values indicating which spaces contain vehicles of the specified make.
     * 
     * A value of true at index n indicates that the n-th space contains a car of the specified make (match),
     * whereas a value of false at index n indicates that the n-th space contains a car of a different make (mismatch).
     */
    boolean[] searchByMake(String make);
}