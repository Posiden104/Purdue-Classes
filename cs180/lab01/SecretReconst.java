public class SecretReconst {
  String secret;
  boolean lock;
  
  public SecretReconst(){
    
  }
  
  public static void main(String[] args){
    Scanner s = new Scanner(System.in);
    secret = s.nextLine();
  }
}