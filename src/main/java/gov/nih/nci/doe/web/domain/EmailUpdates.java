package gov.nih.nci.doe.web.domain;

import java.util.Date;

import javax.persistence.Basic;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

@Entity
@Table(name = "EMAIL_UPDATES_T")
public class EmailUpdates {

	private Integer id;
	private String emailAddress;
	private Date createdDate;

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

		EmailUpdates that = (EmailUpdates) object;

		if (emailAddress != null ? !emailAddress.equals(that.emailAddress) : that.emailAddress != null) {
			return false;
		}

		return createdDate != null ? createdDate.equals(that.createdDate) : that.createdDate == null;

	}

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID", updatable = false, nullable = false)
	public Integer getId() {
		return id;
	}

	public void setId(Integer id) {
		this.id = id;
	}

	@Basic
	@Column(name = "EMAIL_ADDRESS")
	public String getEmailAddress() {
		return emailAddress;
	}

	public void setEmailAddress(String emailAddress) {
		this.emailAddress = emailAddress;
	}

	@Temporal(TemporalType.TIMESTAMP)
	@Column(name = "CREATED_DATE", length = 29)
	public Date getCreatedDate() {
		return createdDate;
	}

	public void setCreatedDate(Date createdDate) {
		this.createdDate = createdDate;
	}

	@Override
	public String toString() {
		return "EmailUpdates{" + ", emailAddress='" + emailAddress + '\'' + ", createdDate=" + createdDate + '}';
	}
}
