package gov.nih.nci.doe.web;

public class DoeWebException extends Exception {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	public DoeWebException() {
		super();
	}

	public DoeWebException(String message) {
		super(message);
	}

	public DoeWebException(String message, Throwable e) {
		super(message, e);
	}

	public DoeWebException(Throwable e) {
		super(e);
	}
	
	public DoeWebException(String message, Integer statusCode) {
		super(message);
	}
}
