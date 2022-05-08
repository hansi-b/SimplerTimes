package org.hansib.simpler_times;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hansib.simpler_times.utils.ResourceLoader;

import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.stage.Stage;

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
	}

	@Override
	public void stop() {
		timesMainController.saveUserSpans();
	}

	public static void main(String[] args) {
		launch(SimplerTimesFx.class, args);
	}
}
