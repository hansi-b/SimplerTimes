package org.hansib.simpler_times.fx;

import java.io.IOException;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hansib.simpler_times.spans.SpansStore;
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

	private SpansStore spansStore;
	private TimesMainController timesMainController;

	@Override
	public void start(Stage primaryStage) throws Exception {

		log.info("Starting ...");
		spansStore = new SpansStore();

		final FXMLLoader fxmlLoader = ResourceLoader.get().getFxmlLoader("timesMain.fxml");
		final Parent root = fxmlLoader.load();
		timesMainController = fxmlLoader.getController();
		timesMainController.setSpans(spansStore.load());

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
		try {
			spansStore.save(timesMainController.getSpans());
		} catch (IOException e) {
			log.error("Could not save spans", e);
		}
	}

	public static void main(String[] args) {
		launch(SimplerTimesFx.class, args);
	}
}
