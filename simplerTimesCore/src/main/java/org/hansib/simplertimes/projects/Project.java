package org.hansib.simplertimes.projects;

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.hansib.sundries.Strings;

public class Project {

	private String name;

	private Project parent;

	private final List<Project> children;

	private Project(Project parent, String name, List<Project> children) {
		this.name = name;
		this.parent = parent;
		this.children = children;
	}

	public static Project rootWithChildren(String name, List<Project> children) {
		Project node = new Project(null, name, new ArrayList<>(children));
		node.children.forEach(c -> c.parent = node);
		return node;
	}

	public static Project root() {
		return new Project(null, null, new ArrayList<>());
	}

	public Project add(String name) {
		Project child = new Project(this, name, new ArrayList<>());
		children.add(child);
		return child;
	}

	public Project remove(Project child) {

		int i = children.indexOf(child);
		if (i < 0)
			throw new IllegalArgumentException(
					String.format("Unknown child %s for %s (have %s)", child, this, children));
		Project removed = children.remove(i);
		removed.parent = null;
		return removed;
	}

	public String name() {
		return name;
	}

	public void setName(String newName) {
		name = newName;
	}

	public String fullProjectName() {
		LinkedList<String> hierarchy = new LinkedList<>();
		for (Project current = this; current.parent != null; current = current.parent) {
			if (current.name != null)
				hierarchy.addFirst(current.name);
		}
		return String.join(" ▸ ", hierarchy);
	}

	public void setProject(String newName) {
		this.name = newName;
	}

	public Project parent() {
		return parent;
	}

	public List<Project> children() {
		return Collections.unmodifiableList(children);
	}

	public Stream<Project> dfStream() {
		Stream<Project> c = children.stream().flatMap(Project::dfStream);
		return Stream.concat(Stream.of(this), c);
	}

	Stream<Project> filter(Set<String> words) {
		if (words.isEmpty())
			return dfStream();
		Set<String> wordsStillMissing = name == null ? words
				: words.stream().filter(w -> !name.contains(w)).collect(Collectors.toSet());
		Stream<Project> filteredChildren = children.stream().flatMap(c -> c.filter(wordsStillMissing));
		return wordsStillMissing.isEmpty() ? Stream.concat(Stream.of(this), filteredChildren) : filteredChildren;
	}

	@Override
	public String toString() {
		return Strings.idStr(this, name);
	}
}
