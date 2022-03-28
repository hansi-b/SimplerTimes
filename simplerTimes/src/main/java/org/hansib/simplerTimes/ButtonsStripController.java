package org.hansib.simplerTimes;

import org.kordamp.ikonli.javafx.FontIcon;

import javafx.animation.Animation;
import javafx.animation.FadeTransition;
import javafx.fxml.FXML;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.util.Duration;

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

		FadeTransition startFt = getFade(startButton);

		startButton.setOnAction(a -> {
			startButton.setDisable(true);
			startFt.play();
			stopButton.setDisable(false);
		});

		stopButton.setOnAction(a -> {
			stopButton.setDisable(true);
			startButton.setDisable(false);
			startFt.stop();
		});
	}


	private static FadeTransition getFade(Button btn) {
		FadeTransition ft = new FadeTransition(Duration.millis(1200), btn);
		ft.setFromValue(.6);
		ft.setToValue(0.15);
		ft.setCycleCount(Animation.INDEFINITE);
		ft.setAutoReverse(true);
		return ft;
	}
}
