package gov.nih.nci.doe.web.model;

import java.util.Objects;

public class KeyValueBean {
    private String key;
    private String value;

    public KeyValueBean(String key, String value) {
        this.key = key;
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
        return key.equals(that.key) &&
            value.equals(that.value);
    }

    public String getKey() {
        return key;
    }

    public String getValue() {
        return value;
    }

    @Override
    public int hashCode() {
        return Objects.hash(key, value);
    }

}
