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
import org.kordamp.ikonli.javafx.FontIcon;

import javafx.application.Platform;
import javafx.fxml.FXML;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.Label;

public class ButtonsStripController {

	private final ScheduledExecutorService scheduler = Executors.newScheduledThreadPool(1);

	private static final Logger log = LogManager.getLogger();

	private static class Icons {

		private static final Node start() {
			return new FontIcon("codicon-play-circle");
		}

		private static final Node stop() {
			return new FontIcon("codicon-stop-circle");
		}
	}

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

		startButton.setOnAction(a -> {
			startButton.setDisable(true);
			stopButton.setDisable(false);

			timer.start();
			scheduleAtFixedRate = scheduler.scheduleAtFixedRate(
					() -> Platform.runLater(() -> elapsedTime.setText(fmt(timer.currentDuration()))), 0, 40,
					TimeUnit.MILLISECONDS);
		});

		stopButton.setOnAction(a -> {
			stopButton.setDisable(true);
			startButton.setDisable(false);

			scheduleAtFixedRate.cancel(true);

			Span span = timer.stopAndGet();
			log.info(span);
		});
	}

	private static String fmt(Duration d) {
		return String.format("%d:%02d:%02d", d.toHours(), d.toMinutesPart(), d.toSecondsPart());
	}
}
