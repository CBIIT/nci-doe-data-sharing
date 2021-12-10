package gov.nih.nci.doe.web.model;

import java.util.Date;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;

@JsonIgnoreProperties(ignoreUnknown = true)
@JsonPropertyOrder({ "success", "error-codes", "challenge_ts", "hostname" })
public class GoogleResponse {

	@JsonProperty("success")
	private Boolean success;

	@JsonProperty("error-codes")
	private String[] errorCode;

	@JsonProperty("challenge_ts")
	private Date challangeDate;

	@JsonProperty("hostname")
	private String hostname;

	public String getHostname() {
		return hostname;
	}

	public void setHostname(String hostname) {
		this.hostname = hostname;
	}

	public Date getChallangeDate() {
		return challangeDate;
	}

	public void setChallangeDate(Date challangeDate) {
		this.challangeDate = challangeDate;
	}

	public Boolean getSuccess() {
		return success;
	}

	public void setSuccess(Boolean success) {
		this.success = success;
	}

	public String[] getErrorCode() {
		return errorCode;
	}

	public void setErrorCode(String[] errorCode) {
		this.errorCode = errorCode;
	}

}
