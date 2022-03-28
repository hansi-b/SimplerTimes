package org.hansib.simplerTimes;

import javafx.fxml.FXML;
import javafx.scene.control.TextArea;

public class TimesMainController {

	@FXML
	TextArea logArea;

	@FXML
	TimesMainController timesMainController;

	@FXML
	void initialize() {
		logArea.setEditable(false);
	}
}
