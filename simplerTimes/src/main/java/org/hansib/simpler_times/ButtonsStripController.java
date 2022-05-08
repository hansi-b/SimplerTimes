package org.hansib.simpler_times;

import java.util.function.Consumer;

import org.hansib.simpler_times.times.Interval;

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

	private TimerDisplay timerDisplay;

	private Consumer<Interval> intervalReceiver;

	@FXML
	void initialize() {
		timerDisplay = new TimerDisplay(elapsedTime);

		startButton.setGraphic(Icons.start());
		stopButton.setGraphic(Icons.stop());
		stopButton.setDisable(true);

		startButton.setOnAction(a -> startTiming());
		stopButton.setOnAction(a -> stopTiming());
	}

	void setIntervalReceiver(Consumer<Interval> intervalReceiver) {
		this.intervalReceiver = intervalReceiver;
	}

	private void startTiming() {
		startButton.setDisable(true);
		stopButton.setDisable(false);
		timerDisplay.start();
	}

	private void stopTiming() {
		stopButton.setDisable(true);
		startButton.setDisable(false);

		intervalReceiver.accept(timerDisplay.stopAndGet());
	}
}
