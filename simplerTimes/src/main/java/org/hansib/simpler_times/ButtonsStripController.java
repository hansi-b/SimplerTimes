package org.hansib.simpler_times;

import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.Label;

public class ButtonsStripController {

	@FXML
	Button startButton;

	@FXML
	Button stopButton;

	@FXML
	Label elapsedTime;

	private final LabelledSpans spans;

	private TimerDisplay timerDisplay;

	public ButtonsStripController() {
		this(new SpanLogger());
	}

	ButtonsStripController(LabelledSpans spans) {
		this.spans = spans;
	}

	@FXML
	void initialize() {
		timerDisplay = new TimerDisplay(elapsedTime);

		startButton.setGraphic(Icons.start());
		stopButton.setGraphic(Icons.stop());
		stopButton.setDisable(true);

		startButton.setOnAction(a -> startTiming());
		stopButton.setOnAction(a -> stopTiming());
	}

	private void startTiming() {
		startButton.setDisable(true);
		stopButton.setDisable(false);
	}

	private void stopTiming() {
		stopButton.setDisable(true);
		startButton.setDisable(false);

		spans.add(LabelledSpan.of("abc", timerDisplay.stopAndGet()));
	}
}
