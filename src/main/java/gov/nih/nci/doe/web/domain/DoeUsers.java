package gov.nih.nci.doe.web.domain;

import java.util.Date;

import javax.persistence.*;

import org.hibernate.annotations.Type;






@Entity
@Table(name = "DOE_USER_T")
public class DoeUsers {

	private Integer id;
	private String firstName;
	private String lastName;
	private String emailAddrr;
	private String password;
	private String institution;
	private Date lockoutDate;
	private Integer lockoutCounter;
	private Boolean isWrite;
	private Boolean isActivated;
	private String uuid;
	private String programName;
	

    public boolean equals(Object object) {
        if (this == object) {
            return true;
        }
        if (object == null || getClass() != object.getClass()) {
            return false;
        }
        if (!super.equals(object)) {
            return false;
        }

        DoeUsers that = (DoeUsers) object;

        if (firstName != null ? !firstName.equals(that.firstName) : that.firstName != null) {
            return false;
        }
        if (lastName != null ? !lastName.equals(that.lastName) : that.lastName != null) {
            return false;
        }
        if (emailAddrr != null ? !emailAddrr.equals(that.emailAddrr) : that.emailAddrr != null) {
            return false;
        }
        if (password != null ? !password.equals(that.password) : that.password != null) {
            return false;
        }

        return institution != null ? institution.equals(that.institution) : that.institution == null;

    }
    
    
    @Id
    @Column(name = "ID", nullable = false, precision = 0)
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "doe_user_seq")
    @SequenceGenerator(name = "doe_user_seq", sequenceName = "doe_user_seq", allocationSize = 1)
	public Integer getId() {
		return id;
	}
	public void setId(Integer id) {
		this.id = id;
	}
	
	@Basic
    @Column(name = "FIRST_NAME")
	public String getFirstName() {
		return firstName;
	}
	public void setFirstName(String firstName) {
		this.firstName = firstName;
	}
	
	@Basic
    @Column(name = "LAST_NAME")
	public String getLastName() {
		return lastName;
	}
	public void setLastName(String lastName) {
		this.lastName = lastName;
	}
	
	@Basic
    @Column(name = "EMAIL_ADDR")
	public String getEmailAddrr() {
		return emailAddrr;
	}
	public void setEmailAddrr(String emailAddrr) {
		this.emailAddrr = emailAddrr;
	}
	
	@Basic
    @Column(name = "PASSWORD")
	public String getPassword() {
		return password;
	}
	public void setPassword(String password) {
		this.password = password;
	}
	
	@Basic
    @Column(name = "INSTITUTION")
	public String getInstitution() {
		return institution;
	}
	public void setInstitution(String institution) {
		this.institution = institution;
	}
	
	@Temporal(TemporalType.TIMESTAMP)
	@Column(name = "lockout_date", length = 29)
	public Date getLockoutDate() {
		return lockoutDate;
	}

	public void setLockoutDate(Date lockoutDate) {
		this.lockoutDate = lockoutDate;
	}

	@Column(name = "LOCKOUT_COUNTER", precision = 4, scale = 0)
	public Integer getLockoutCounter() {
		return lockoutCounter;
	}

	public void setLockoutCounter(Integer lockoutCounter) {
		this.lockoutCounter = lockoutCounter;
	}
	
	
	@Basic
	@Column(name = "IS_WRITE", nullable = true, length = 1)
	@Type(type = "yes_no")
    public Boolean getIsWrite() {
		return isWrite;
	}


	public void setIsWrite(Boolean isWrite) {
		this.isWrite = isWrite;
	}

	@Basic
	@Column(name = "IS_ACTIVATED", nullable = true, length = 1)
	@Type(type = "yes_no")
	public Boolean getIsActivated() {
		return isActivated;
	}


	public void setIsActivated(Boolean isActivated) {
		this.isActivated = isActivated;
	}

	@Basic
    @Column(name = "UUID")
	public String getUuid() {
		return uuid;
	}


	public void setUuid(String uuid) {
		this.uuid = uuid;
	}


	@Basic
    @Column(name = "PROGRAM_NAME")
	public String getProgramName() {
		return programName;
	}


	public void setProgramName(String programName) {
		this.programName = programName;
	}


	@Override
    public String toString() {
        return "DoeUsers{" +
            ", firstName='" + firstName + '\'' +
            ", lastName='" + lastName + '\'' +
            ", emailAddrr=" + emailAddrr +
            ", password='" + password + '\'' +
            ", institution=" + institution +
             ", lockoutDate=" + institution +
              ", lockoutCounter=" + institution +
            '}';
    }
}
