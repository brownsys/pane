package paneclient;
public class InvalidResvException extends Exception{
	
	private static final long serialVersionUID = 1L;

	public InvalidResvException(String message) {
		super(message);
	}
	
	public InvalidResvException() {
		super();
	}


}