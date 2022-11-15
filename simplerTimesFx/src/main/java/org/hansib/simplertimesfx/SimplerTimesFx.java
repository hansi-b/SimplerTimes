package org.hansib.simplertimesfx;

import java.io.IOException;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hansib.simplertimes.AppData;
import org.hansib.simplertimes.projects.Project;
import org.hansib.simplertimes.yaml.ProjectStore;
import org.hansib.simplertimes.yaml.SpansStore;
import org.hansib.sundries.ResourceLoader;
import org.hansib.sundries.fx.FxmlControllerLoader;

import com.dustinredmond.fxtrayicon.FXTrayIcon;

import javafx.application.Application;
import javafx.application.Platform;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;
import javafx.stage.Stage;
import javafx.stage.Window;
import javafx.stage.WindowEvent;

public class SimplerTimesFx extends Application {

	private static final Logger log = LogManager.getLogger();

	private final FxmlControllerLoader fxControllerLoader = new FxmlControllerLoader();

	private SpansStore spansStore;
	private ProjectStore treeStore;

	private TimesMainController timesMainController;

	@Override
	public void start(Stage primaryStage) throws Exception {

		log.info("Starting ...");
		spansStore = new SpansStore();
		treeStore = new ProjectStore(new AppData().dataPath("projects.yml"));

		timesMainController = fxControllerLoader.loadAndGetController("timesMain.fxml",
				(Parent root) -> primaryStage.setScene(new Scene(root)));

		Project projectRoot = treeStore.load();
		timesMainController.setProjects(projectRoot);
		timesMainController.setSpans(spansStore.load(projectRoot));

		primaryStage.setTitle("SimplerTimes");
		primaryStage.show();

		primaryStage.addEventHandler(KeyEvent.KEY_PRESSED, ev -> {
			if (ev.getCode() == KeyCode.Q && ev.isControlDown()) {
				Window window = primaryStage.getScene().getWindow();
				window.fireEvent(new WindowEvent(window, WindowEvent.WINDOW_CLOSE_REQUEST));
				Platform.exit();
			}
		});

		FXTrayIcon trayIcon = new FXTrayIcon(primaryStage, new ResourceLoader().getResourceUrl("logo.png"));
		trayIcon.show();
	}

	@Override
	public void stop() {
		try {
			spansStore.save(timesMainController.getSpans());
			treeStore.save(timesMainController.getProjects());
		} catch (IOException e) {
			log.error("Could not save spans", e);
		}
	}

	public static void main(String[] args) {
		launch(SimplerTimesFx.class, args);
	}
}
