package org.hansib.simpler_times;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hansib.simpler_times.spans.Span;
import org.hansib.simpler_times.spans.SpansCollection;
import org.hansib.simpler_times.times.Interval;

import javafx.fxml.FXML;
import javafx.scene.control.TextField;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;

public class TimesMainController {

	private static final Logger log = LogManager.getLogger();

	@FXML
	TextField projectField;

	@FXML
	ButtonsStripController buttonsStripController;

	private SpansCollection spans;

	@FXML
	void initialize() {
		buttonsStripController.setIntervalReceiver(this::handleInterval);

		projectField.addEventHandler(KeyEvent.KEY_PRESSED, e -> {
			if (e.getCode() == KeyCode.ENTER) {
				buttonsStripController.startButton.requestFocus();
				buttonsStripController.startButton.fire();
			}
		});
	}

	void setSpans(SpansCollection spans) {
		this.spans = spans;
	}

	SpansCollection getSpans() {
		return spans;
	}

	void handleInterval(Interval t) {
		log.info("Got interval: {} {}", projectField.getText(), t);
		try {
			spans.add(new Span(projectField.getText(), t.start(), t.end()));
		} catch (IllegalArgumentException ex) {
			log.info("Ignoring invalid span: {}", ex.getMessage());
		}
	}
}
