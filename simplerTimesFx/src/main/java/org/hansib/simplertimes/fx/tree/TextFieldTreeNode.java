package org.hansib.simplertimes.fx.tree;

public interface TextFieldTreeNode<T extends TextFieldTreeNode<T>> {

	String text();

	void setText(String newText);

	void remove();

	T addChild(String childText);

	Iterable<T> children();
}