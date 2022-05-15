package org.hansib.simpler_times.tree;

import javafx.scene.control.ContextMenu;
import javafx.scene.control.MenuItem;
import javafx.scene.control.TextField;
import javafx.scene.control.TreeCell;
import javafx.scene.control.TreeItem;
import javafx.scene.input.KeyCode;

class TextFieldTreeCellImpl extends TreeCell<String> {

	private final ContextMenu contextMenu;
	private TextField textField;

	public TextFieldTreeCellImpl() {
		contextMenu = createContextMenu();
	}

	private ContextMenu createContextMenu() {
		ContextMenu addMenu = new ContextMenu();
		MenuItem addMenuItem = new MenuItem("Add Node");
		addMenuItem.setOnAction(t -> addItem());
		addMenu.getItems().add(addMenuItem);
		return addMenu;
	}

	private void addItem() {
		TreeItem<String> treeItem = getTreeItem();
		if (treeItem == null) {
			return;
		}
		treeItem.getChildren().add(new TreeItem<>("___"));
	}

	@Override
	public void startEdit() {
		super.startEdit();

		if (textField == null) {
			createTextField();
		}
		setText(null);
		setGraphic(textField);
		textField.selectAll();
	}

	@Override
	public void cancelEdit() {
		super.cancelEdit();

		setText(getItem());
		setGraphic(getTreeItem().getGraphic());
	}

	@Override
	public void updateItem(String item, boolean empty) {
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
			setContextMenu(contextMenu);
		}
	}

	private void createTextField() {
		textField = new TextField(getString());
		textField.setOnKeyReleased(t -> {
			if (t.getCode() == KeyCode.ENTER) {
				commitEdit(textField.getText());
			} else if (t.getCode() == KeyCode.ESCAPE) {
				cancelEdit();
			}
		});
	}

	private String getString() {
		String i = getItem();
		return i == null ? "" : i;
	}
}