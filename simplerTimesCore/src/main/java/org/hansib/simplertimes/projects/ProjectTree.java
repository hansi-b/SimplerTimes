package org.hansib.simplertimes.projects;

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.hansib.sundries.Strings;

public class ProjectTree {

	private String name;

	private ProjectTree parent;

	private final List<ProjectTree> children;

	public ProjectTree(ProjectTree parent, String name, List<ProjectTree> children) {
		this.name = name;
		this.parent = parent;
		this.children = children;
	}

	public static ProjectTree connected(String name, List<ProjectTree> children) {
		ProjectTree node = new ProjectTree(null, name, new ArrayList<>(children));
		node.children.forEach(c -> c.parent = node);
		return node;
	}

	public static ProjectTree root() {
		return new ProjectTree(null, null, new ArrayList<>());
	}

	public ProjectTree add(String name) {
		ProjectTree child = new ProjectTree(this, name, new ArrayList<>());
		children.add(child);
		return child;
	}

	public ProjectTree remove(ProjectTree child) {

		int i = children.indexOf(child);
		if (i < 0)
			throw new IllegalArgumentException(
					String.format("Unknown child %s for %s (have %s)", child, this, children));
		ProjectTree removed = children.remove(i);
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
		for (ProjectTree current = this; current.parent != null; current = current.parent) {
			if (current.name != null)
				hierarchy.addFirst(current.name);
		}
		return String.join(" ▸ ", hierarchy);
	}

	public void setProject(String newName) {
		this.name = newName;
	}

	public ProjectTree parent() {
		return parent;
	}

	public List<ProjectTree> children() {
		return Collections.unmodifiableList(children);
	}

	public Stream<ProjectTree> dfStream() {
		Stream<ProjectTree> c = children.stream().flatMap(ProjectTree::dfStream);
		return Stream.concat(Stream.of(this), c);
	}

	Stream<ProjectTree> filter(Set<String> words) {
		if (words.isEmpty())
			return dfStream();
		Set<String> wordsStillMissing = name == null ? words
				: words.stream().filter(w -> !name.contains(w)).collect(Collectors.toSet());
		Stream<ProjectTree> filteredChildren = children.stream().flatMap(c -> c.filter(wordsStillMissing));
		return wordsStillMissing.isEmpty() ? Stream.concat(Stream.of(this), filteredChildren) : filteredChildren;
	}

	@Override
	public String toString() {
		return Strings.idStr(this, name);
	}
}
