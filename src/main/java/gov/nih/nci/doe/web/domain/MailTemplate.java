package gov.nih.nci.doe.web.domain;

import java.io.Serializable;
import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;


@Entity
@Table(name = "MAIL_TEMPLATE_T")
public class MailTemplate implements Serializable {

	private static final long serialVersionUID = 3092597482427044459L;

	private String id;
	private String shortIdentifier;
	private String emailTitle;
	private String emailSubject;
	private String emailBody;
	private String createdBy;
	private Date createdDate;
	
	public MailTemplate() {
		
	}

	public MailTemplate(String id, String shortIdentifier, String emailTitle,
			String emailSubject, String emailBody) {
		this.id = id;
		this.shortIdentifier = shortIdentifier;
		this.emailTitle = emailTitle;
		this.emailSubject = emailSubject;
		this.emailBody = emailBody;
	}
	
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "id", unique = true, nullable = false)
	public String getId() {
		return id;
	}
	
	public void setId(String id) {
		this.id = id;
	}
	
	@Column(name = "SHORT_IDENTIFIER", nullable = false, length = 200)
	public String getShortIdentifier() {
		return shortIdentifier;
	}
	
	public void setShortIdentifier(String shortIdentifier) {
		this.shortIdentifier = shortIdentifier;
	}
	
	@Column(name = "EMAIL_TITLE", nullable = false, length = 300)
	public String getEmailTitle() {
		return emailTitle;
	}
	
	public void setEmailTitle(String emailTitle) {
		this.emailTitle = emailTitle;
	}
	
	@Column(name = "EMAIL_SUBJECT", nullable = false, length = 800)
	public String getEmailSubject() {
		return emailSubject;
	}
	
	public void setEmailSubject(String emailSubject) {
		this.emailSubject = emailSubject;
	}
	
	@Column(name = "EMAIL_BODY", nullable = false, length = 4000)
	public String getEmailBody() {
		return emailBody;
	}
	
	public void setEmailBody(String emailBody) {
		this.emailBody = emailBody;
	}
	
	@Column(name = "created_by", nullable = false, length = 50)
	public String getCreatedBy() {
		return createdBy;
	}
	
	public void setCreatedBy(String createdBy) {
		this.createdBy = createdBy;
	}
	
	@Temporal(TemporalType.TIMESTAMP)
	@Column(name = "created_date", nullable = false, length = 29)
	public Date getCreatedDate() {
		return createdDate;
	}
	
	public void setCreatedDate(Date createdDate) {
		this.createdDate = createdDate;
	}
	
}
