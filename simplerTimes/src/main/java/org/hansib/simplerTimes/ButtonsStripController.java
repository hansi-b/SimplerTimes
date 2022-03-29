package org.hansib.simplerTimes;

import org.kordamp.ikonli.javafx.FontIcon;

import javafx.fxml.FXML;
import javafx.scene.Node;
import javafx.scene.control.Button;

public class ButtonsStripController {

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
	void initialize() {
		startButton.setGraphic(Icons.start());
		stopButton.setGraphic(Icons.stop());
		stopButton.setDisable(true);

		startButton.setOnAction(a -> {
			startButton.setDisable(true);
			stopButton.setDisable(false);
		});

		stopButton.setOnAction(a -> {
			stopButton.setDisable(true);
			startButton.setDisable(false);
		});
	}
}
