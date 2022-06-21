package org.hansib.simplertimes.projects;

public class Project {

	private String name;

	public Project(String name) {
		this.name = name;
	}

	public String name() {
		return name;
	}

	public void set(String newName) {
		this.name = newName;
	}

	@Override
	public String toString() {
		return String.format("Project[%s]", name);
	}

}
