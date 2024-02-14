/**
 * SimplerTimes - https://github.com/hansi-b/SimplerTimes
 *
 * Copyright (C) 2022-2023 Hans Bering
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
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

	boolean canMoveTo(T newParent);

	void moveTo(T newParent);

	public static <T extends TreeItemNode<T>> TreeItem<T> linkTree(T node) {
		TreeItem<T> treeItem = new TreeItem<>(node);
		node.children().forEach(c -> treeItem.getChildren().add(linkTree(c)));
		return treeItem;
	}
}