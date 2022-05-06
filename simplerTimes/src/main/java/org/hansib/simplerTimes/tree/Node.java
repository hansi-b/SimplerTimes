package org.hansib.simplerTimes.tree;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.hansib.sundries.Strings;

public class Node<E> {

	private E element;

	private Node<E> parent;

	private List<Node<E>> children;

	private Node(Node<E> parent, E element, List<Node<E>> children) {
		this.element = element;
		this.parent = parent;
		this.children = children;
	}

	public static <F> Node<F> root() {
		return new Node<>(null, null, new ArrayList<>());
	}

	public Node<E> add(E element) {
		Node<E> child = new Node<>(this, element, new ArrayList<>());
		children.add(child);
		return child;
	}

	public Node<E> remove(Node<E> child) {

		int i = children.indexOf(child);
		if (i < 0)
			throw new IllegalArgumentException(
					String.format("Unknown child %s for %s (have %s)", child, this, children));
		Node<E> removed = children.remove(i);
		removed.parent = null;
		return removed;
	}

	public E element() {
		return element;
	}

	public void setElement(E element) {
		this.element = element;
	}

	public Node<E> parent() {
		return parent;
	}

	public List<Node<E>> children() {
		return Collections.unmodifiableList(children);
	}

	public String toString() {
		return Strings.idStr(this, String.valueOf(element));
	}
}
