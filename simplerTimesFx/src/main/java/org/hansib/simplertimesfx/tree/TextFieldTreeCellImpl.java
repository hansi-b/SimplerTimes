package org.hansib.simplertimesfx.tree;

import java.util.function.Supplier;

import org.hansib.simplertimes.tree.Nameable;
import org.hansib.simplertimes.tree.TreeNode;

import javafx.application.Platform;
import javafx.scene.control.ContextMenu;
import javafx.scene.control.MenuItem;
import javafx.scene.control.TextField;
import javafx.scene.control.TreeCell;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeView;
import javafx.scene.input.KeyCode;

class TextFieldTreeCellImpl<E extends Nameable> extends TreeCell<TreeNode<E>> {

	private TextField textField;
	private ContextMenu contextMenu;

	public TextFieldTreeCellImpl(TreeView<TreeNode<E>> treeview, Supplier<E> nodeSupplier) {
		contextMenu = new ContextMenu();
		contextMenu.getItems().add(getAddMenuItem(treeview, nodeSupplier));
		contextMenu.getItems().add(getRemoveMenuItem());
	}

	private MenuItem getAddMenuItem(TreeView<TreeNode<E>> treeview, Supplier<E> elementSupplier) {
		MenuItem addMenuItem = new MenuItem("Add Subproject");
		addMenuItem.setOnAction(t -> {
			TreeItem<TreeNode<E>> i = getTreeItem();
			TreeItem<TreeNode<E>> current = i != null ? i : treeview.getRoot();
			TreeNode<E> nodeChild = current.getValue().add(elementSupplier.get());
			TreeItem<TreeNode<E>> newItem = new TreeItem<>(nodeChild);
			current.getChildren().add(newItem);
			treeview.getSelectionModel().select(newItem);
			treeview.edit(newItem);
		});
		return addMenuItem;
	}

	private MenuItem getRemoveMenuItem() {
		MenuItem removeMenuItem = new MenuItem("Remove Subproject");
		removeMenuItem.setOnAction(t -> {
			TreeItem<TreeNode<E>> current = getTreeItem();
			TreeNode<E> nodeChild = current.getValue();
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
		textField.setText(getString());
		setText(null);
		setGraphic(textField);
		textField.requestFocus();
	}

	@Override
	public void cancelEdit() {
		super.cancelEdit();

		setText(getString());
		setGraphic(getTreeItem().getGraphic());
	}

	@Override
	public void updateItem(TreeNode<E> item, boolean empty) {
		super.updateItem(item, empty);

		if (empty) {
			setText(null);
			setGraphic(null);
		} else if (isEditing()) {
			if (textField != null) {
				textField.setText(getString());
			}
			setText(null);
			setGraphic(textField);
		} else {
			setText(getString());
			setGraphic(getTreeItem().getGraphic());
			if (getTreeItem().getParent() != null) {
				setContextMenu(contextMenu);
			}
		}
	}

	private void createTextField() {
		textField = new TextField(getString());
		textField.setOnKeyPressed(t -> {
			if (t.getCode() == KeyCode.ESCAPE) {
				textField.setText(getString());
				cancelEdit();
			}
		});
		textField.setOnKeyReleased(t -> {
			if (t.getCode() == KeyCode.ENTER) {
				getItem().element().set(textField.getText());
				commitEdit(getItem());
			}
		});
		textField.focusedProperty().addListener((observable, oldValue, newValue) -> {
			if (Boolean.FALSE.equals(newValue)) {
				getItem().element().set(textField.getText());
				commitEdit(getItem());
			} else if (Boolean.TRUE.equals(newValue)) {
				Platform.runLater(() -> {
					if (!textField.getText().isEmpty())
						textField.selectAll();
				});
			}
		});
	}

	private String getString() {
		E e = getItem().element();
		return e == null ? "-" : e.name();
	}
}