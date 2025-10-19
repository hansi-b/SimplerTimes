/*-
 * GPL for SimplerTimes - https://github.com/hansi-b/SimplerTimes
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

import java.awt.GraphicsEnvironment;
import java.awt.SystemTray;

import javafx.application.Application;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;
import javafx.stage.Stage;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hansib.simplertimes.DataStore;
import org.hansib.simplertimes.fx.data.ObservableData;
import org.hansib.simplertimes.fx.l10n.L10nSetup;
import org.hansib.simplertimes.prefs.AppPrefs;
import org.hansib.sundries.fx.ControllerLoader;
import org.hansib.sundries.fx.StageData;
import org.hansib.sundries.fx.StageToggle;

public class SimplerTimesFx extends Application {

	private static final Logger log = LogManager.getLogger();

	private DataStore dataStore;
	private ObservableData data;

	private AppPrefs prefs;

	@Override
	public void start(Stage primaryStage) throws Exception {

		log.info("Starting ...");
		final ExitManager exitManager = ExitManager.get();

		L10nSetup.activateEnglish();

		prefs = AppPrefs.get();
		DisclaimerChecker.checkDisclaimer(prefs.disclaimer, exitManager::exit);

		dataStore = new DataStore();
		data = ObservableData.load(dataStore);

		TimesMainController timesMainController = new ControllerLoader<TimesMainController>().loadFxmlAndGetController(
			"timesMain.fxml", () -> new TimesMainController(data),
			(Parent root) -> primaryStage.setScene(new Scene(root)));

		primaryStage.setTitle(AppNameWindowTitle.fmt());
		new Resources().loadLogo(logo -> primaryStage.getIcons().add(logo));

		primaryStage.addEventHandler(KeyEvent.KEY_PRESSED, ev -> {
			if (ev.getCode() == KeyCode.Q && ev.isControlDown())
				exitManager.exit();
		});

		if (isSystemTrayMenuSupported()) {
			new TrayIconMenu(data, timesMainController.getRecorder(), primaryStage, exitManager).show();
		} else {
			log.info("System tray menu not supported.");
			primaryStage.setOnCloseRequest(e -> exitManager.exit());
		}
		new StageToggle(() -> primaryStage).toggle();
		primaryStage.sizeToScene();

		prefs.windows.mainWindow.apply(primaryStage);
		exitManager.addPreExitAction(() -> prefs.windows.mainWindow = StageData.of(primaryStage));
	}

	private static boolean isSystemTrayMenuSupported() {
		return SystemTray.isSupported() && !GraphicsEnvironment.isHeadless();
		// && System.getProperty("os.name").toLowerCase().contains("win");
	}

	@Override
	public void stop() {
		log.info("Stopping ...");
		data.store(dataStore);
		prefs.save();
	}

	public static void main(String[] args) {
		launch(SimplerTimesFx.class, args);
	}
}
