package gov.nih.nci.doe.web.model;

import java.util.Date;

public class DoeUsersModel {

	private String firstName;
	private String lastName;
	private String emailAddrr;
	private String institution;
	private Date lockoutDate;
	private Integer lockoutCounter;
	private Boolean isWrite;
	private String programName;

	public String getFirstName() {
		return firstName;
	}
	public void setFirstName(String firstName) {
		this.firstName = firstName;
	}

	public String getLastName() {
		return lastName;
	}
	public void setLastName(String lastName) {
		this.lastName = lastName;
	}
	
	public String getEmailAddrr() {
		return emailAddrr;
	}
	public void setEmailAddrr(String emailAddrr) {
		this.emailAddrr = emailAddrr;
	}

	public String getInstitution() {
		return institution;
	}
	public void setInstitution(String institution) {
		this.institution = institution;
	}
	public Date getLockoutDate() {
		return lockoutDate;
	}
	public void setLockoutDate(Date lockoutDate) {
		this.lockoutDate = lockoutDate;
	}
	public Integer getLockoutCounter() {
		return lockoutCounter;
	}
	public void setLockoutCounter(Integer lockoutCounter) {
		this.lockoutCounter = lockoutCounter;
	}
	public Boolean getIsWrite() {
		return isWrite;
	}
	public void setIsWrite(Boolean isWrite) {
		this.isWrite = isWrite;
	}
	public String getProgramName() {
		return programName;
	}
	public void setProgramName(String programName) {
		this.programName = programName;
	}
	
	
}
