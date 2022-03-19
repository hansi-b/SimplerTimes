package org.hansib.simplerTimes;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hansib.simplerTimes.utils.IWin32SystemMonitorListener;
import org.hansib.simplerTimes.utils.LogoffHandler;
import org.hansib.simplerTimes.utils.ResourceLoader;
import org.hansib.simplerTimes.utils.Win32SystemMonitor;

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

		new LogoffHandler();
		log("Started.");
	}

	void addListener() {
		Win32SystemMonitor.addListener(new IWin32SystemMonitorListener() {
			public void onMachineLogon() {
				log("Logon");
			}

			public void onMachineLogoff() {
				log("Logoff");
			}

			public void onMachineUnlocked() {
				log("Unlocked");
			}

			public void onMachineLocked() {
				log("Locked");
			}

			public void onMachineGoingToSuspend() {
				log("Suspend");
			}

			public void onOther(int wParam, int lParam) {
				log("Other: %s / %s", wParam, lParam);
			}

			public void onOtherPowerChange(int wParam, int lParam) {
				log("Other power change: %s / %s", wParam, lParam);
			}
		});
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
