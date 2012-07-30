
public class PaneException extends Exception {
	
	private static final long serialVersionUID = 1L;

	public PaneException(String message) {
		super(message);
	}
	
	public static enum Type{
		INVALIDNEWUSER,
		INVALIDAUTHTICATE,
		INVALIDGRANT,
		INVALIDALLOW,
		INVALIDDENY,
		INVALIDRESV,
		INVALIDNEWSHARE
	};
	
	public static PaneException create(Type type, String message){
		switch (type) {
		case INVALIDNEWUSER:
			return new InvalidNewUserException(message);
		case INVALIDAUTHTICATE:
			return new InvalidAuthenticateException(message);
		case INVALIDGRANT:
			return new InvalidGrantException(message);
		case INVALIDALLOW:
			return new InvalidAllowException(message);
		case INVALIDDENY:
			return new InvalidDenyException(message);
		case INVALIDRESV:
			return new InvalidResvException(message);
		case INVALIDNEWSHARE:
			return new InvalidNewShareException(message);
		default:
			throw new IllegalArgumentException("Invalid exception type");
		}
	}
	
	public static class InvalidNewUserException extends PaneException {		
		public InvalidNewUserException(String message) {
			super(message);
		}
	}
	
	public static class InvalidAuthenticateException extends PaneException {		
		public InvalidAuthenticateException(String message) {
			super(message);
		}
	}
	
	public static class InvalidGrantException extends PaneException {
		public InvalidGrantException(String message) {
			super(message);
		}
	}
	
	public static class InvalidAllowException extends PaneException {		
		public InvalidAllowException(String message) {
			super(message);
		}
	}
	
	public static class InvalidDenyException extends PaneException {		
		public InvalidDenyException(String message) {
			super(message);
		}
	}
	
	public static class InvalidResvException extends PaneException {		
		public InvalidResvException(String message) {
			super(message);
		}
	}
	
	public static class InvalidNewShareException extends PaneException {
		public InvalidNewShareException(String message) {
			super(message);
		}
	}
}
