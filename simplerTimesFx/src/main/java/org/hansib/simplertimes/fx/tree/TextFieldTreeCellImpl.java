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

import java.util.function.Function;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javafx.application.Platform;
import javafx.scene.control.ContextMenu;
import javafx.scene.control.TextField;
import javafx.scene.control.TreeCell;
import javafx.scene.input.KeyCode;

class TextFieldTreeCellImpl<T extends TextNode> extends TreeCell<T> { // NOSONAR

	private static final Logger log = LogManager.getLogger();

	private TextField textField;
	private Function<TextFieldTreeCellImpl<T>, ContextMenu> cellContextMenuFunction;

	private final Runnable changeHandler;

	public TextFieldTreeCellImpl(Runnable changeHandler) {
		super();
		this.changeHandler = changeHandler;
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
		log.info("Commit new text in {}", this);
		commitEdit(getItem());
		changeHandler.run();
	}

	private String itemText() {
		return String.valueOf(getItem().text());
	}
}