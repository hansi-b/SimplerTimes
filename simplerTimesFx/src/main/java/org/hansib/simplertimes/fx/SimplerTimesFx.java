/**
 * SimplerTimes - https://github.com/hansi-b/SimplerTimes
 *
 * Copyright (C) 2022-2023 Hans Bering
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.hansib.simplertimes.fx;

import static org.hansib.simplertimes.fx.l10n.General.AppNameWindowTitle;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hansib.simplertimes.AppPrefs;
import org.hansib.simplertimes.DataStore;
import org.hansib.simplertimes.fx.data.ObservableData;
import org.hansib.simplertimes.fx.l10n.L10nSetup;
import org.hansib.sundries.fx.FxResourceLoader;

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

	private final FxResourceLoader fxLoader = new FxResourceLoader();

	private Stage primaryStage;

	private DataStore dataStore;

	private TimesMainController timesMainController;

	@Override
	public void start(Stage primaryStage) throws Exception {

		log.info("Starting ...");
		this.primaryStage = primaryStage;

		L10nSetup.activateEnglish();

		AppPrefs prefs = AppPrefs.create();
		DisclaimerChecker.checkDisclaimer(prefs.disclaimerAccepted(), this::fireCloseRequest);

		timesMainController = fxLoader.loadFxmlAndGetController("timesMain.fxml",
				(Parent root) -> primaryStage.setScene(new Scene(root)));

		dataStore = new DataStore();
		ObservableData data = ObservableData.load(dataStore);
		timesMainController.setData(data);

		primaryStage.setTitle(AppNameWindowTitle.fmt());
		new Resources().loadLogo(logo -> primaryStage.getIcons().add(logo));

		primaryStage.addEventHandler(KeyEvent.KEY_PRESSED, ev -> {
			if (ev.getCode() == KeyCode.Q && ev.isControlDown())
				fireCloseRequest();
		});

		if (isWindows()) {
			TrayIconMenu.create(primaryStage, data, timesMainController.getRecorder());
		} else {
			primaryStage.setOnCloseRequest(event -> Platform.exit());
		}
		primaryStage.show();
	}

	private void fireCloseRequest() {
		Window window = primaryStage.getScene().getWindow();
		window.fireEvent(new WindowEvent(window, WindowEvent.WINDOW_CLOSE_REQUEST));
	}

	/**
	 * @return true iff we are on Windows
	 */
	private static boolean isWindows() {
		return System.getProperty("os.name").toLowerCase().contains("win");
	}

	@Override
	public void stop() {
		log.info("Stopping ...");
		timesMainController.getData().store(dataStore);
	}

	public static void main(String[] args) {
		launch(SimplerTimesFx.class, args);
	}
}
