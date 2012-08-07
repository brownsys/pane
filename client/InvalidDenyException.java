package paneclient;

public class InvalidDenyException extends Exception {
	
	private static final long serialVersionUID = 1L;

	public InvalidDenyException(String message) {
		super(message);
	}
	
	public InvalidDenyException() {
		super();
	}
	
}
