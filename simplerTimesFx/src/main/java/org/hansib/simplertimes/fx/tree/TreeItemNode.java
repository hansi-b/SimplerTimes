/*-
 * GPL for SimplerTimes - https://github.com/hansi-b/SimplerTimes
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

import java.util.Comparator;
import java.util.stream.Stream;

import javafx.scene.control.TreeItem;

public interface TreeItemNode<T extends TreeItemNode<T>> extends TextNode {

	interface PreRemovalCallback<T> {

		/**
		 * Called when the tree view is asked to remove the argument node. Should check and prepare the imminent removal
		 * of the argument node. Can return false to cancel the removal. <p/>
		 *
		 * NB: The implementation must not call {@link TreeItemNode#remove()} on the argument - this is done by the
		 * tree.
		 *
		 * @param node the node submitted for removal
		 * @return true if the removal of the argument node from the tree can proceed; if false, the node is not removed
		 */
		boolean removalAccepted(T node);
	}

	void remove();

	T addChild(String childText);

	Stream<T> children();

	/**
	 * Sorts the children of this node by their texts according to the argument comparator.
	 */
	void sortChildren(Comparator<String> comparator);

	/**
	 * Checks the preconditions for moving this project to the argument parent project at the argument child list
	 * index.
	 * <ol>
	 * <li>Both projects must be in the same project tree (have the same id
	 * generator).
	 * <li>The new parent must not be the same as or a descendant of this project.
	 * <li>If the new parent is the current parent, the argument index must not be
	 * this project's index in the children list.
	 * </ol>
	 *
	 * @param newParent the project which would become the new parent
	 * @param newIndex  the new index in the new parent's children list
	 * @return true if this project can be moved to become a child of the argument node
	 */
	boolean canMoveTo(T newParent, int newIndex);

	/**
	 * Makes this project to the argument parent project at the argument child list index.
	 *
	 * @param newParent the new parent for this project
	 * @param newIndex  the target index in the new parent's child list; if this item is already in that list, then the
	 *                  index is interpreted as the index in the list with the current item first removed from the list
	 */
	void moveTo(T newParent, int newIndex);

	static <T extends TreeItemNode<T>> TreeItem<T> linkTree(T node) {
		TreeItem<T> treeItem = new TreeItem<>(node);
		node.children().forEach(c -> treeItem.getChildren().add(linkTree(c)));
		return treeItem;
	}
}