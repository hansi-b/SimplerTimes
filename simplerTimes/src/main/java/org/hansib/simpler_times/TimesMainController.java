package org.hansib.simpler_times;

import java.io.IOException;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hansib.simpler_times.spans.Span;
import org.hansib.simpler_times.spans.SpansCollection;
import org.hansib.simpler_times.spans.UserSpans;
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

	private final UserSpans userSpans;
	private final SpansCollection spans;

	public TimesMainController() {
		userSpans = new UserSpans();
		spans = userSpans.getUserSpans();
	}

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

	private void handleInterval(Interval t) {
		log.info("{} {}", projectField.getText(), t);
		spans.add(new Span(projectField.getText(), t.start(), t.end()));
	}

	public void saveUserSpans() {
		try {
			userSpans.saveSpans(spans);
		} catch (IOException e) {
			log.error("Could not save user spans", e);
		}
	}
}
