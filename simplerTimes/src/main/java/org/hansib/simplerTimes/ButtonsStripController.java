package org.hansib.simplerTimes;

import java.time.Duration;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hansib.simplerTimes.times.Span;
import org.hansib.simplerTimes.times.Timer;

import javafx.application.Platform;
import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.Label;

public class ButtonsStripController {

	private final ScheduledExecutorService scheduler = Executors.newScheduledThreadPool(1);

	private static final Logger log = LogManager.getLogger();

	@FXML
	Button startButton;

	@FXML
	Button stopButton;

	@FXML
	Label elapsedTime;

	private final Timer timer;

	private ScheduledFuture<?> scheduleAtFixedRate;

	public ButtonsStripController() {
		timer = new Timer();
	}

	@FXML
	void initialize() {

		startButton.setGraphic(Icons.start());
		stopButton.setGraphic(Icons.stop());
		stopButton.setDisable(true);

		startButton.setOnAction(a -> startTiming());
		stopButton.setOnAction(a -> stopTiming());
	}

	private void startTiming() {
		startButton.setDisable(true);
		stopButton.setDisable(false);
		timer.start();
		scheduleAtFixedRate = scheduler.scheduleAtFixedRate(
				() -> Platform.runLater(() -> elapsedTime.setText(fmtTime(timer.currentDuration()))), 0, 40,
				TimeUnit.MILLISECONDS);
	}

	private void stopTiming() {
		stopButton.setDisable(true);
		startButton.setDisable(false);

		scheduleAtFixedRate.cancel(true);

		Span span = timer.stopAndGet();
		log.info(span);
	}

	private static String fmtTime(Duration d) {
		return String.format("%d:%02d:%02d", d.toHours(), d.toMinutesPart(), d.toSecondsPart());
	}
}
