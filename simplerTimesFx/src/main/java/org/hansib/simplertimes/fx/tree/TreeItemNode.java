package org.hansib.simplertimes.fx.tree;

import javafx.scene.control.TreeItem;

public interface TreeItemNode<T extends TreeItemNode<T>> {

	interface PreRemovalCallback<T> {

		/**
		 * Called when the tree view is asked to remove the argument node. Should check
		 * and prepare the imminent removal of the argument node. Can return false to
		 * cancel the removal.
		 * 
		 * NB: The implementation must not call {@link TreeItemNode#remove()} on the
		 * argument - this is done by the tree.
		 * 
		 * @param node the node submitted for removal
		 * @return true if the removal of the argument node from the tree can proceed;
		 *         if false, the node is not removed
		 */
		boolean removalAccepted(T node);
	}

	void remove();

	T addChild(String childText);

	Iterable<T> children();

	public static <T extends TreeItemNode<T>> TreeItem<T> linkTree(T node) {
		TreeItem<T> treeItem = new TreeItem<>(node);
		node.children().forEach(c -> treeItem.getChildren().add(linkTree(c)));
		return treeItem;
	}
}