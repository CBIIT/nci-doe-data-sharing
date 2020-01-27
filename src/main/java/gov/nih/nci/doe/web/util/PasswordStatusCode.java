package gov.nih.nci.doe.web.util;

public enum PasswordStatusCode {
	SUCCESS, 
	// any error/exception
	UNKNOWN_ERROR,
	// statuses for validation
	// must contain at least 1 capital letter, at least 1 lower case letter, at least 1 numeric character and at least 1 special character
	INVALID_FORMAT,
	// must be at least eight (8) characters in length and no more than 14 characters
	INVALID_LENGTH,
	// password cannot be reused
	NEW_PASSWD_SAME_AS_PREV_PASSWD;
}
