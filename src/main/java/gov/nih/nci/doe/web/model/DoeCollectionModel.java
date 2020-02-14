package gov.nih.nci.doe.web.model;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import gov.nih.nci.hpc.domain.datamanagement.HpcCollection;

public class DoeCollectionModel {
	private String path;
	private String isDataObject;

	private HpcCollection collection;
	private List<DoeMetadataAttrEntry> selfMetadataEntries;
	private List<DoeMetadataAttrEntry> parentMetadataEntries;
	private Map<String, String> attributes = new HashMap<String, String>();
	private String defaultValue;
	private List<String> validValues;
	
	public String getDefaultValue() {
		return defaultValue;
	}

	public void setDefaultValue(String defaultValue) {
		this.defaultValue = defaultValue;
	}

	public List<String> getValidValues() {
		return validValues;
	}

	public void setValidValues(List<String> validValues) {
		this.validValues = validValues;
	}

	public Map<String, String> getAttributes() {
		return attributes;
	}

	public void setAttributes(Map<String, String> attributes) {
		this.attributes = attributes;
	}

	public void addAttribute(String attribute, String value) {
		this.attributes.put(attribute, value);
	}

	public String getPath() {
		return path;
	}

	public void setPath(String path) {
		this.path = path;
	}

	public HpcCollection getCollection() {
		return collection;
	}

	public void setCollection(HpcCollection collection) {
		this.collection = collection;
	}

	public List<DoeMetadataAttrEntry> getSelfMetadataEntries() {
		if (selfMetadataEntries == null)
			selfMetadataEntries = new ArrayList<DoeMetadataAttrEntry>();
		Collections.sort(selfMetadataEntries, new Comparator<DoeMetadataAttrEntry>() {
			@Override
			public int compare(DoeMetadataAttrEntry entry1, DoeMetadataAttrEntry entry2) {

				return entry1.getAttrName().compareTo(entry2.getAttrName());
			}
		});
		return selfMetadataEntries;
	}

	public void setSelfMetadataEntries(List<DoeMetadataAttrEntry> metadata) {
		this.selfMetadataEntries = metadata;
	}

	public List<DoeMetadataAttrEntry> getParentMetadataEntries() {
		if (parentMetadataEntries == null)
			parentMetadataEntries = new ArrayList<DoeMetadataAttrEntry>();
		Collections.sort(parentMetadataEntries, new Comparator<DoeMetadataAttrEntry>() {
			@Override
			public int compare(DoeMetadataAttrEntry entry1, DoeMetadataAttrEntry entry2) {
				if (entry1.getAttrName() != null && entry2.getAttrName() != null)
					return entry1.getAttrName().compareTo(entry2.getAttrName());
				else
					return -1;
			}
		});
		return parentMetadataEntries;
	}

	public void setParentMetadataEntries(List<DoeMetadataAttrEntry> parentMetadataEntries) {
		this.parentMetadataEntries = parentMetadataEntries;
	}
	
	public String isDataObject() {
		return isDataObject;
	}

	public void setDataObject(String isDataObject) {
		this.isDataObject = isDataObject;
	}

}
