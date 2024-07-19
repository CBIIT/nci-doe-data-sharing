package gov.nih.nci.doe.web.model;

import java.util.ArrayList;
import java.util.List;

public class DoeCollectionEntry {

	private String name;

	private String fullPath;

	private List<DoeCollectionEntry> children;

	private boolean populated;

	private boolean partial;

	public boolean isPopulated() {
		return populated;
	}

	public void setPopulated(boolean populated) {
		this.populated = populated;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public List<DoeCollectionEntry> getChildren() {
		if (children == null)
			children = new ArrayList<DoeCollectionEntry>();
		return children;
	}

	public void setChildren(List<DoeCollectionEntry> children) {
		this.children = children;
	}

	public String getFullPath() {
		return fullPath;
	}

	public void setFullPath(String fullPath) {
		this.fullPath = fullPath;
	}

	public boolean isPartial() {
		return partial;
	}

	public void setPartial(boolean partial) {
		this.partial = partial;
	}

}
