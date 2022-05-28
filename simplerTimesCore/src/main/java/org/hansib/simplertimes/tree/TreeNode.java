package org.hansib.simplertimes.tree;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.hansib.sundries.Strings;

public class TreeNode<E extends Nameable> {

	private E element;

	private TreeNode<E> parent;

	private final List<TreeNode<E>> children;

	TreeNode(TreeNode<E> parent, E element, List<TreeNode<E>> children) {
		this.element = element;
		this.parent = parent;
		this.children = children;
	}

	public static <F extends Nameable> TreeNode<F> root() {
		return new TreeNode<>(null, null, new ArrayList<>());
	}

	public TreeNode<E> add(E element) {
		TreeNode<E> child = new TreeNode<>(this, element, new ArrayList<>());
		children.add(child);
		return child;
	}

	public TreeNode<E> remove(TreeNode<E> child) {

		int i = children.indexOf(child);
		if (i < 0)
			throw new IllegalArgumentException(
					String.format("Unknown child %s for %s (have %s)", child, this, children));
		TreeNode<E> removed = children.remove(i);
		removed.parent = null;
		return removed;
	}

	public E element() {
		return element;
	}

	public void setElement(E element) {
		this.element = element;
	}

	public TreeNode<E> parent() {
		return parent;
	}

	public List<TreeNode<E>> children() {
		return Collections.unmodifiableList(children);
	}

	@Override
	public String toString() {
		return Strings.idStr(this, String.valueOf(element));
	}
}
