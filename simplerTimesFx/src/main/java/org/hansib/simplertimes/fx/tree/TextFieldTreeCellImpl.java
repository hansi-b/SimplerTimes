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

import org.hansib.simplertimes.fx.l10n.MenuItems;
import org.hansib.sundries.fx.ContextMenuBuilder;

import javafx.application.Platform;
import javafx.scene.control.ContextMenu;
import javafx.scene.control.TextField;
import javafx.scene.control.TreeCell;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeView;
import javafx.scene.input.KeyCode;

class TextFieldTreeCellImpl<T extends TextFieldTreeNode<T>> extends TreeCell<T> { // NOSONAR

	private TextField textField;
	private final ContextMenu contextMenu;

	public TextFieldTreeCellImpl(TreeView<T> treeview) {
		contextMenu = new ContextMenuBuilder() //
				.item(MenuItems.NewSubproject.fmt(), e -> TreeViewWindow.newTreeItem(treeview, getTreeItem())) //
				.item(MenuItems.RemoveSubproject.fmt(), e -> removeItem()) //
				.build();
	}

	private void removeItem() {
		TreeItem<T> current = getTreeItem();
		current.getValue().remove();
		current.getParent().getChildren().remove(current);
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
			if (getTreeItem().getParent() != null) {
				setContextMenu(contextMenu);
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