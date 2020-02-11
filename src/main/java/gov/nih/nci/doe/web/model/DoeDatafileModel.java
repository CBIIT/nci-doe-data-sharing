package gov.nih.nci.doe.web.model;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import gov.nih.nci.hpc.domain.datamanagement.HpcDataObject;

public class DoeDatafileModel {
	private String path;
	private HpcDataObject dataObject;
	private List<DoeMetadataAttrEntry> selfMetadataEntries;
	private List<DoeMetadataAttrEntry> parentMetadataEntries;

	public String getPath() {
		return path;
	}

	public void setPath(String path) {
		this.path = path;
	}

	public HpcDataObject getDataObject() {
		return dataObject;
	}

	public void setDataObject(HpcDataObject dataObject) {
		this.dataObject = dataObject;
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

}
