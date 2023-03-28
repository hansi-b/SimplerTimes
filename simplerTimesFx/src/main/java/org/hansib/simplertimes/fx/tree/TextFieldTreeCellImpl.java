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

import java.util.function.Supplier;

import org.hansib.simplertimes.fx.l10n.MenuItems;
import org.hansib.simplertimes.projects.Project;

import javafx.application.Platform;
import javafx.scene.control.ContextMenu;
import javafx.scene.control.MenuItem;
import javafx.scene.control.TextField;
import javafx.scene.control.TreeCell;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeView;
import javafx.scene.input.KeyCode;

class TextFieldTreeCellImpl extends TreeCell<Project> {

	private TextField textField;
	private ContextMenu contextMenu;

	public TextFieldTreeCellImpl(TreeView<Project> treeview, Supplier<String> nodeSupplier) {
		contextMenu = new ContextMenu();
		contextMenu.getItems().add(getAddMenuItem(treeview, nodeSupplier));
		contextMenu.getItems().add(getRemoveMenuItem());
	}

	private MenuItem getAddMenuItem(TreeView<Project> treeview, Supplier<String> elementSupplier) {
		MenuItem addMenuItem = new MenuItem(MenuItems.NewSubproject.fmt());
		addMenuItem.setOnAction(t -> {
			TreeItem<Project> i = getTreeItem();
			TreeItem<Project> current = i != null ? i : treeview.getRoot();
			Project nodeChild = current.getValue().add(elementSupplier.get());
			TreeItem<Project> newItem = new TreeItem<>(nodeChild);
			current.getChildren().add(newItem);
			treeview.getSelectionModel().select(newItem);
			treeview.edit(newItem);
		});
		return addMenuItem;
	}

	private MenuItem getRemoveMenuItem() {
		MenuItem removeMenuItem = new MenuItem(MenuItems.RemoveSubproject.fmt());
		removeMenuItem.setOnAction(t -> {
			TreeItem<Project> current = getTreeItem();
			Project nodeChild = current.getValue();
			nodeChild.parent().remove(nodeChild);
			current.getParent().getChildren().remove(current);
		});
		return removeMenuItem;
	}

	@Override
	public void startEdit() {
		super.startEdit();
		if (textField == null) {
			createTextField();
		}
		textField.setText(itemName());
		setText(null);
		setGraphic(textField);
		textField.requestFocus();
	}

	@Override
	public void cancelEdit() {
		super.cancelEdit();

		setText(itemName());
		setGraphic(getTreeItem().getGraphic());
	}

	@Override
	public void updateItem(Project item, boolean empty) {
		super.updateItem(item, empty);

		if (empty) {
			setText(null);
			setGraphic(null);
		} else if (isEditing()) {
			if (textField != null) {
				textField.setText(itemName());
			}
			setText(null);
			setGraphic(textField);
		} else {
			setText(itemName());
			setGraphic(getTreeItem().getGraphic());
			if (getTreeItem().getParent() != null) {
				setContextMenu(contextMenu);
			}
		}
	}

	private void createTextField() {
		textField = new TextField(itemName());
		textField.setOnKeyPressed(t -> {
			if (t.getCode() == KeyCode.ESCAPE) {
				textField.setText(itemName());
				cancelEdit();
			}
		});
		textField.setOnKeyReleased(t -> {
			if (t.getCode() == KeyCode.ENTER) {
				getItem().setName(textField.getText());
				commitEdit(getItem());
			}
		});
		textField.focusedProperty().addListener((observable, oldValue, newValue) -> {
			if (Boolean.FALSE.equals(newValue)) {
				getItem().setName(textField.getText());
				commitEdit(getItem());
			} else if (Boolean.TRUE.equals(newValue)) {
				Platform.runLater(() -> {
					if (!textField.getText().isEmpty())
						textField.selectAll();
				});
			}
		});
	}

	private String itemName() {
		return String.valueOf(getItem().name());
	}
}