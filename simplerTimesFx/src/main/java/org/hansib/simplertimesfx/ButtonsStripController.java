package org.hansib.simplertimesfx;

import java.time.Duration;
import java.util.function.Consumer;

import org.hansib.simplertimes.times.DurationTicker;
import org.hansib.simplertimes.times.Interval;

import javafx.application.Platform;
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

	private DurationTicker timerDisplay;

	private Consumer<Interval> intervalReceiver;

	@FXML
	void initialize() {
		timerDisplay = new DurationTicker(this::updateTime);

		startButton.setGraphic(Icons.start());
		stopButton.setGraphic(Icons.stop());

		stopButton.setDisable(true);

		startButton.setOnAction(a -> startTiming());
		stopButton.setOnAction(a -> stopTiming());
		updateTime(Duration.ZERO);
	}

	private void updateTime(Duration duration) {
		Platform.runLater(() -> elapsedTime.setText(
				String.format("%d:%02d:%02d", duration.toHours(), duration.toMinutesPart(), duration.toSecondsPart())));
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
