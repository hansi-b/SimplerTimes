package org.hansib.simplertimes.tree;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.hansib.sundries.Strings;

public class TreeNode {

	private Project project;

	private TreeNode parent;

	private final List<TreeNode> children;

	public TreeNode(TreeNode parent, Project element, List<TreeNode> children) {
		this.project = element;
		this.parent = parent;
		this.children = children;
	}

	public static TreeNode connected(Project element, List<TreeNode> children) {
		TreeNode node = new TreeNode(null, element, new ArrayList<>(children));
		node.children.forEach(c -> c.parent = node);
		return node;
	}

	public static TreeNode root() {
		return new TreeNode(null, null, new ArrayList<>());
	}

	public TreeNode add(Project project) {
		TreeNode child = new TreeNode(this, project, new ArrayList<>());
		children.add(child);
		return child;
	}

	public TreeNode remove(TreeNode child) {

		int i = children.indexOf(child);
		if (i < 0)
			throw new IllegalArgumentException(
					String.format("Unknown child %s for %s (have %s)", child, this, children));
		TreeNode removed = children.remove(i);
		removed.parent = null;
		return removed;
	}

	public Project project() {
		return project;
	}

	public void setProject(Project project) {
		this.project = project;
	}

	public TreeNode parent() {
		return parent;
	}

	public List<TreeNode> children() {
		return Collections.unmodifiableList(children);
	}

	public Stream<TreeNode> dfStream() {
		Stream<TreeNode> c = children.stream().flatMap(TreeNode::dfStream);
		return Stream.concat(Stream.of(this), c);
	}

	Stream<TreeNode> filter(Set<String> words) {
		if (words.isEmpty())
			return dfStream();
		Set<String> wordsStillMissing = project == null ? words
				: words.stream().filter(w -> !project.name().contains(w)).collect(Collectors.toSet());
		Stream<TreeNode> filteredChildren = children.stream().flatMap(c -> c.filter(wordsStillMissing));
		return wordsStillMissing.isEmpty() ? Stream.concat(Stream.of(this), filteredChildren) : filteredChildren;
	}

	@Override
	public String toString() {
		return Strings.idStr(this, String.valueOf(project));
	}
}
