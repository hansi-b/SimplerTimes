package org.hansib.simplerTimes;

import javafx.fxml.FXML;
import javafx.scene.control.TextArea;

public class TimesMainController {

	@FXML
	TextArea logArea;

	@FXML
	void initialize() {
		logArea.setEditable(false);
	}
}
