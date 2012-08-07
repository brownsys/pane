package paneclient;

public class InvalidAuthenticateException extends Exception{
	
	private static final long serialVersionUID = 1L;

	public InvalidAuthenticateException(String message) {
		super(message);
	}
	
	public InvalidAuthenticateException() {
		super();
	}

}
