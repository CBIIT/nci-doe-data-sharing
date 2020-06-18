package gov.nih.nci.doe.web.model;

import java.util.Objects;

public class KeyValueBean {
    private String key;
    private String value;
    private String displayName;

    public KeyValueBean(String key, String value) {
        this.key = key;
        this.value = value;
    }

    public KeyValueBean(String key,String displayName,String value) {
        this.key = key;
        this.displayName = displayName;
        this.value = value;
    }
    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        KeyValueBean that = (KeyValueBean) o;
        return key.equals(that.key) && displayName.equals(that.displayName) &&
            value.equals(that.value);
    }

    public String getKey() {
        return key;
    }

    public String getValue() {
        return value;
    }

    
    public String getDisplayName() {
		return displayName;
	}

	public void setDisplayName(String displayName) {
		this.displayName = displayName;
	}

	@Override
    public int hashCode() {
        return Objects.hash(key,displayName, value);
    }

}
