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

import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Function;

import org.hansib.simplertimes.fx.data.FxProject;

import javafx.application.Platform;
import javafx.scene.control.ContextMenu;
import javafx.scene.control.TextField;
import javafx.scene.control.TreeCell;
import javafx.scene.control.TreeItem;
import javafx.scene.input.ClipboardContent;
import javafx.scene.input.Dragboard;
import javafx.scene.input.KeyCode;
import javafx.scene.input.TransferMode;

class TextFieldTreeCellImpl<T extends TextNode> extends TreeCell<T> { // NOSONAR

	private TextField textField;
	private Function<TextFieldTreeCellImpl<T>, ContextMenu> cellContextMenuFunction;

	public TextFieldTreeCellImpl() {
		super();
	}

	TextFieldTreeCellImpl<T> withDragAndDrop(AtomicReference<TreeItem<T>> draggedItemHolder) {
		setOnDragDetected(event -> {
			handleDragDetected(draggedItemHolder);
			event.consume();
		});
		setOnDragOver(event -> {
			event.acceptTransferModes(TransferMode.MOVE);
			event.consume();
		});
		setOnDragDropped(event -> {
			handleDragDropped(draggedItemHolder);
			event.consume();
		});

		return this;
	}

	private void handleDragDetected(AtomicReference<TreeItem<T>> draggedItemHolder) {
		TreeItem<T> draggedItem = getTreeItem();
		if (draggedItem == null)
			return;

		draggedItemHolder.set(draggedItem);

		ClipboardContent content = new ClipboardContent();
		content.putString(getTreeItem().getValue().text());

		Dragboard dragboard = getTreeView().startDragAndDrop(TransferMode.MOVE);
		dragboard.setContent(content);
	}

	private void handleDragDropped(AtomicReference<TreeItem<T>> draggedItemHolder) {
		TreeItem<T> draggedItem = draggedItemHolder.get();
		if (draggedItem == null || !(draggedItem.getValue() instanceof FxProject sourceProject))
			return;

		TreeItem<T> targetItem = getTreeItem();
		T value;
		if (targetItem == null) {
			targetItem = draggedItem;
			while (targetItem.getParent() != null)
				targetItem = targetItem.getParent();
		}
		value = targetItem.getValue();
		if (value instanceof FxProject targetProject && sourceProject.canMoveTo(targetProject)) {
			sourceProject.moveTo(targetProject);
			draggedItem.getParent().getChildren().remove(draggedItem);
			targetItem.getChildren().add(draggedItem);
		}
		draggedItemHolder.set(null);
	}

	public TextFieldTreeCellImpl<T> withContextMenu(
			Function<TextFieldTreeCellImpl<T>, ContextMenu> cellContextMenuFunction) {
		this.cellContextMenuFunction = cellContextMenuFunction;
		return this;
	}

	@Override
	public void startEdit() {
		super.startEdit();
		if (textField == null) {
			createTextField();
		}
		textField.setText(itemText());
		setText(null);
		setGraphic(textField);
		textField.requestFocus();
	}

	@Override
	public void cancelEdit() {
		super.cancelEdit();

		setText(itemText());
		setGraphic(getTreeItem().getGraphic());
	}

	@Override
	public void updateItem(T item, boolean empty) {
		super.updateItem(item, empty);
		setContextMenu(null);

		if (empty) {
			setText(null);
			setGraphic(null);
		} else if (isEditing()) {
			if (textField != null) {
				textField.setText(itemText());
			}
			setText(null);
			setGraphic(textField);
		} else {
			setText(itemText());
			setGraphic(getTreeItem().getGraphic());
			if (cellContextMenuFunction != null) {
				setContextMenu(cellContextMenuFunction.apply(this));
			}
		}
	}

	private void createTextField() {
		textField = new TextField(itemText());
		textField.setOnKeyPressed(t -> {
			if (t.getCode() == KeyCode.ESCAPE) {
				textField.setText(itemText());
				cancelEdit();
			}
		});
		textField.setOnKeyReleased(t -> {
			if (t.getCode() == KeyCode.ENTER) {
				commitNewText();
			}
		});
		textField.focusedProperty().addListener((observable, oldValue, newValue) -> {
			if (Boolean.FALSE.equals(newValue)) {
				commitNewText();
			} else if (Boolean.TRUE.equals(newValue)) {
				Platform.runLater(() -> {
					if (!textField.getText().isEmpty())
						textField.selectAll();
				});
			}
		});
	}

	private void commitNewText() {
		getItem().setText(textField.getText());
		commitEdit(getItem());
	}

	private String itemText() {
		return String.valueOf(getItem().text());
	}
}