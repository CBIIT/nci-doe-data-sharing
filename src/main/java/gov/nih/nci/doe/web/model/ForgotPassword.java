package gov.nih.nci.doe.web.model;

public class ForgotPassword {


	private String emailAddrr;

	private String password;

	private String confirmPassword;
		
	public String getPassword() {
		return password;
	}
	
	public void setPassword(String password) {
		this.password = password;
	}
	
	public String getEmailAddrr() {
		return emailAddrr;
	}

	public void setEmailAddrr(String emailAddrr) {
		this.emailAddrr = emailAddrr;
	}


	public String getConfirmPassword() {
		return confirmPassword;
	}

	public void setConfirmPassword(String confirmPassword) {
		this.confirmPassword = confirmPassword;
	}
	
}
