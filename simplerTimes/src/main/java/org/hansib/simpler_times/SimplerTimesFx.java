package org.hansib.simpler_times;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hansib.simpler_times.utils.ResourceLoader;

import javafx.application.Application;
import javafx.application.Platform;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;
import javafx.stage.Stage;
import javafx.stage.Window;
import javafx.stage.WindowEvent;

public class SimplerTimesFx extends Application {

	private static final Logger log = LogManager.getLogger();

	private TimesMainController timesMainController;

	@Override
	public void start(Stage primaryStage) throws Exception {

		log.info("Starting ...");

		final FXMLLoader fxmlLoader = ResourceLoader.get().getFxmlLoader("timesMain.fxml");
		final Parent root = fxmlLoader.load();
		timesMainController = fxmlLoader.getController();

		primaryStage.setTitle("SimplerTimes");
		primaryStage.setScene(new Scene(root));
		primaryStage.show();
		primaryStage.addEventHandler(KeyEvent.KEY_PRESSED, ev -> {
			if (ev.getCode() == KeyCode.Q && ev.isControlDown()) {
				Window window = primaryStage.getScene().getWindow();
				window.fireEvent(new WindowEvent(window, WindowEvent.WINDOW_CLOSE_REQUEST));
				Platform.exit();
			}
		});
	}

	@Override
	public void stop() {
		timesMainController.saveUserSpans();
	}

	public static void main(String[] args) {
		launch(SimplerTimesFx.class, args);
	}
}
