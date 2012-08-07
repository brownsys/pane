package paneclient;

public class InvalidAllowException extends Exception{
	
	private static final long serialVersionUID = 1L;

	public InvalidAllowException(String message) {
		super(message);
	}
	
	public InvalidAllowException() {
		super();
	}


}