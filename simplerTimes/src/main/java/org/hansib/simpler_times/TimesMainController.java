package org.hansib.simpler_times;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hansib.simpler_times.spans.SpansCollection;
import org.hansib.simpler_times.times.Interval;

import javafx.fxml.FXML;
import javafx.scene.control.TextField;

public class TimesMainController {

	private static final Logger log = LogManager.getLogger();

	@FXML
	TextField projectField;

	@FXML
	ButtonsStripController buttonsStripController;

	private final SpansCollection spans;

	TimesMainController() {
		this(new SpansCollection());
	}

	TimesMainController(SpansCollection spans) {
		this.spans = spans;
	}

	@FXML
	void initialize() {
		buttonsStripController.setIntervalReceiver(this::handleInterval);
	}

	private void handleInterval(Interval t) {
		log.info("{} {}", projectField.getText(), t);
	}
}
