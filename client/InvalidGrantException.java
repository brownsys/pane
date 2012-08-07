package paneclient;

public class InvalidGrantException extends Exception {
	
	private static final long serialVersionUID = 1L;

	public InvalidGrantException(String message) {
		super(message);
	}
	
	public InvalidGrantException() {
		super();
	}
	
}
