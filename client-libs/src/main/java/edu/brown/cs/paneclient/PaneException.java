package edu.brown.cs.paneclient;

@SuppressWarnings("serial")
public abstract class PaneException extends Exception {
	
	public PaneException(String msg) {
		super(msg);
	}
	
	public static class InvalidAllowException extends PaneException {
		public InvalidAllowException(String msg) {
			super(msg);
		}
	}
	
	public static class InvalidAuthenticateException extends PaneException {
		public InvalidAuthenticateException(String msg) {
			super(msg);
		}
	}
	
	public static class InvalidDenyException extends PaneException {
		public InvalidDenyException(String msg) {
			super(msg);
		}
	}
	
	public static class InvalidGrantException extends PaneException {
		public InvalidGrantException(String msg) {
			super(msg);
		}
	}
	
	public static class InvalidNewShareException extends PaneException {
		public InvalidNewShareException(String msg) {
			super(msg);
		}
	}
	
	public static class InvalidResvException extends PaneException {
		public InvalidResvException(String msg) {
			super(msg);
		}
	}
	
	public static class InvalidUserException extends PaneException {
		public InvalidUserException(String msg) {
			super(msg);
		}
	}
}
