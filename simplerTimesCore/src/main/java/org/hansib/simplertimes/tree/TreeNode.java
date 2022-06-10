package org.hansib.simplertimes.tree;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.hansib.sundries.Strings;

public class TreeNode {

	private Project element;

	private TreeNode parent;

	private final List<TreeNode> children;

	public TreeNode(TreeNode parent, Project element, List<TreeNode> children) {
		this.element = element;
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

	public TreeNode add(Project element) {
		TreeNode child = new TreeNode(this, element, new ArrayList<>());
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

	public Project element() {
		return element;
	}

	public void setElement(Project element) {
		this.element = element;
	}

	public TreeNode parent() {
		return parent;
	}

	public List<TreeNode> children() {
		return Collections.unmodifiableList(children);
	}

	@Override
	public String toString() {
		return Strings.idStr(this, String.valueOf(element));
	}
}
