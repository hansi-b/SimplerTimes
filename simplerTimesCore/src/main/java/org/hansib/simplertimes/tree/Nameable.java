package org.hansib.simplertimes.tree;

public class Nameable {

	private String name;

	public Nameable(String name) {
		this.name = name;
	}

	public String name() {
		return name;
	}

	public void set(String newName) {
		this.name = newName;
	}
}