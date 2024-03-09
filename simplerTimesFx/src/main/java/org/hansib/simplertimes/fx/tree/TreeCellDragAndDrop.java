/**
 * SimplerTimes - https://github.com/hansi-b/SimplerTimes
 *
 * Copyright (C) 2022-2024 Hans Bering
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

import java.util.concurrent.atomic.AtomicReference;

import org.hansib.sundries.fx.Styler;

import javafx.scene.control.TreeCell;
import javafx.scene.control.TreeItem;
import javafx.scene.input.ClipboardContent;
import javafx.scene.input.DragEvent;
import javafx.scene.input.Dragboard;
import javafx.scene.input.TransferMode;

class TreeCellDragAndDrop<T extends TreeItemNode<T>> {

	private static final String CSS_CAN_DRAG_TO_ABOVE = "can-drag-to-above";
	private static final String CSS_CAN_DRAG_TO_CHILD = "can-drag-to-child";
	private static final String CSS_CAN_DRAG_TO_BELOW = "can-drag-to-below";

	private enum DragRelation {
		Below(.7, CSS_CAN_DRAG_TO_BELOW), Child(.3, CSS_CAN_DRAG_TO_CHILD), Above(.0, CSS_CAN_DRAG_TO_ABOVE);

		private final double threshold;
		private final String css;

		private DragRelation(double threshold, String css) {
			this.threshold = threshold;
			this.css = css;
		}

		static DragRelation of(DragEvent event, TreeCell<?> cell) {
			double yFraction = event.getY() / cell.getBoundsInParent().getHeight();
			if (yFraction >= Below.threshold)
				return Below;
			if (yFraction >= Child.threshold)
				return Child;
			return Above;
		}

		String css() {
			return css;
		}
	}

	private record DragTarget<T extends TreeItemNode<T>>(TreeItem<T> targetItem, int targetIndex,
			DragRelation relation) {
	}

	private final AtomicReference<TreeItem<T>> draggedItemHolder;

	TreeCellDragAndDrop() {
		draggedItemHolder = new AtomicReference<>(null);
	}

	TreeCell<T> withDragAndDrop(TreeCell<T> cell) {

		final Styler styler = new Styler(cell);

		cell.setOnDragDetected(event -> {
			handleDragDetected(cell);
			cell.getTreeView().getSelectionModel().select(cell.getTreeItem());
			event.consume();
		});
		cell.setOnDragOver(event -> {
			styler.removeAll(CSS_CAN_DRAG_TO_ABOVE, CSS_CAN_DRAG_TO_CHILD, CSS_CAN_DRAG_TO_BELOW);
			if (canDragTo(cell, event) instanceof DragTarget<T> target) {
				styler.add(target.relation().css());
			}
			event.acceptTransferModes(TransferMode.MOVE);
			event.consume();
		});
		cell.setOnDragExited(event -> {
			styler.removeAll(CSS_CAN_DRAG_TO_ABOVE, CSS_CAN_DRAG_TO_CHILD, CSS_CAN_DRAG_TO_BELOW);
			event.consume();
		});
		cell.setOnDragDropped(event -> {
			styler.removeAll(CSS_CAN_DRAG_TO_ABOVE, CSS_CAN_DRAG_TO_CHILD, CSS_CAN_DRAG_TO_BELOW);
			handleDragDropped(cell, event);
			event.consume();
		});
		return cell;
	}

	private void handleDragDetected(TreeCell<T> cell) {
		TreeItem<T> draggedItem = cell.getTreeItem();
		if (draggedItem == null)
			return;

		draggedItemHolder.set(draggedItem);

		ClipboardContent content = new ClipboardContent();
		content.putString(String.valueOf(cell.getText() != null ? cell.getText() : cell.getTreeItem().getValue()));

		Dragboard dragboard = cell.getTreeView().startDragAndDrop(TransferMode.MOVE);
		dragboard.setContent(content);
	}

	private void handleDragDropped(TreeCell<T> cell, DragEvent event) {
		TreeItem<T> draggedItem = draggedItemHolder.get();
		if (draggedItem == null)
			return;

		if (canDragTo(cell, event) instanceof DragTarget<T> target) {
			T source = draggedItem.getValue();
			source.moveTo(target.targetItem().getValue(), target.targetIndex());
			draggedItem.getParent().getChildren().remove(draggedItem);
			target.targetItem().getChildren().add(target.targetIndex(), draggedItem);
		}

		cell.getTreeView().getSelectionModel().select(draggedItem);
		draggedItemHolder.set(null);
	}

	private DragTarget<T> canDragTo(TreeCell<T> cell, DragEvent event) {
		TreeItem<T> draggedItem = draggedItemHolder.get();
		if (draggedItem == null)
			return null;

		var dragRelation = DragRelation.of(event, cell);
		TreeItem<T> targetItem = findTargetItem(cell, draggedItem);
		int targetIndex = 0;
		if (dragRelation != DragRelation.Child && targetItem.getParent() != null) {
			TreeItem<T> parent = targetItem.getParent();
			targetIndex = dragRelation == DragRelation.Above ? parent.getChildren().indexOf(cell.getTreeItem())
					: parent.getChildren().indexOf(cell.getTreeItem()) + 1;
			targetItem = parent;
		}
		T source = draggedItem.getValue();
		T target = targetItem.getValue();
		return source.canMoveTo(target, targetIndex) ? new DragTarget<>(targetItem, targetIndex, dragRelation) : null;
	}

	/**
	 * @return either the argument cell's item, if not null; otherwise (for empty
	 *         cells) the root of the dragged item
	 */
	private TreeItem<T> findTargetItem(TreeCell<T> cell, TreeItem<T> draggedItem) {
		TreeItem<T> targetItem = cell.getTreeItem();
		if (targetItem == null) {
			targetItem = draggedItem;
			while (targetItem.getParent() != null)
				targetItem = targetItem.getParent();
		}
		return targetItem;
	}
}