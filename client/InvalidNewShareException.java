package paneclient;

public class InvalidNewShareException extends Exception {
	
	private static final long serialVersionUID = 1L;

	public InvalidNewShareException(String message) {
		super(message);
	}
	
	public InvalidNewShareException() {
		super();
	}
	
}
