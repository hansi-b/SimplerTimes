package org.hansib.simplertimes.fx.tree;

import javafx.scene.control.TreeItem;

public interface TreeItemNode<T extends TreeItemNode<T>> {

	void remove();

	T addChild(String childText);

	Iterable<T> children();

	public static <T extends TreeItemNode<T>> TreeItem<T> linkTree(T node) {
		TreeItem<T> treeItem = new TreeItem<>(node);
		node.children().forEach(c -> treeItem.getChildren().add(linkTree(c)));
		return treeItem;
	}
}