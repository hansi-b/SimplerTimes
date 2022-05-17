package org.hansib.simplertimesfx;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javafx.scene.control.TextField;
import javafx.scene.control.TreeCell;

class TreeCellFactory extends TreeCell<String> {

	private static final Logger log = LogManager.getLogger();

	private TextField textField;

	public TreeCellFactory() {
	}

	@Override
	public void startEdit() {
		super.startEdit();
		log.info("startEdit '{}'", getItem());
		if (textField == null) {
			createTextField();
		}
		setText(null);
		setGraphic(textField);
	}

	private void createTextField() {
		textField = new TextField("...");
	}

	@Override
	public void cancelEdit() {
		super.cancelEdit();
		log.info("cancelEdit '{}'", getItem());
	}

	@Override
	public void updateItem(String item, boolean empty) {
		super.updateItem(item, empty);

		log.info("updateItem: '{}' '{}' ({})", item, empty, getItem());
	}
}