package org.hansib.simplerTimes;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hansib.simplerTimes.utils.ResourceLoader;

import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.stage.Stage;
import javafx.stage.WindowEvent;

public class SimplerTimesFx extends Application {

	private static final Logger log = LogManager.getLogger();

	private TimesMainController appController;

	@Override
	public void start(Stage primaryStage) throws Exception {

		log("Starting ...");

		final FXMLLoader fxmlLoader = ResourceLoader.get().getFxmlLoader("timesMain.fxml");
		final Parent root = fxmlLoader.load();
		appController = fxmlLoader.getController();

		primaryStage.setTitle("SimplerTimes");
		primaryStage.setScene(new Scene(root));
		primaryStage.show();

		primaryStage.getScene().getWindow().addEventFilter(WindowEvent.ANY, e -> {
			log("Received %s", e);
		});

		log("Started.");
	}

	void log(String fmt, Object... args) {
		String msg = String.format(fmt, args);
		if (appController != null)
			appController.logArea.appendText(msg + "\n");
		log.info(msg);
	}

	public static void main(String[] args) {
		launch(SimplerTimesFx.class, args);
	}
}
