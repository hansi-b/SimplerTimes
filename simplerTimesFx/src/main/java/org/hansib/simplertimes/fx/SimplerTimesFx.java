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

import static org.hansib.simplertimes.fx.l10n.L10nKeys.AppNameWindowTitle;

import java.io.IOException;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hansib.simplertimes.AppData;
import org.hansib.simplertimes.AppPrefs;
import org.hansib.simplertimes.fx.l10n.L10nSetup;
import org.hansib.simplertimes.fx.l10n.MenuItems;
import org.hansib.simplertimes.projects.Project;
import org.hansib.simplertimes.yaml.ProjectStore;
import org.hansib.simplertimes.yaml.SpansStore;
import org.hansib.sundries.fx.FxResourceLoader;

import com.dustinredmond.fxtrayicon.FXTrayIcon;

import javafx.application.Application;
import javafx.application.Platform;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.image.Image;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;
import javafx.stage.Stage;
import javafx.stage.Window;
import javafx.stage.WindowEvent;

public class SimplerTimesFx extends Application {

	private static final Logger log = LogManager.getLogger();

	private final FxResourceLoader fxLoader = new FxResourceLoader();

	private Stage primaryStage;

	private SpansStore spansStore;
	private ProjectStore treeStore;

	private TimesMainController timesMainController;

	@Override
	public void start(Stage primaryStage) throws Exception {

		log.info("Starting ...");
		this.primaryStage = primaryStage;

		L10nSetup.activateEnglish();

		AppPrefs prefs = AppPrefs.create();
		DisclaimerChecker.checkDisclaimer(prefs.disclaimerAccepted(), this::fireCloseRequest);

		AppData appData = AppData.atDefault();
		spansStore = new SpansStore(appData.spansPath());
		treeStore = new ProjectStore(appData.projectsPath());

		Image logo = fxLoader.loadImage("logo.png");
		if (logo == null)
			log.warn("Could not load application icon");
		else
			primaryStage.getIcons().add(logo);

		timesMainController = fxLoader.loadFxmlAndGetController("timesMain.fxml",
				(Parent root) -> primaryStage.setScene(new Scene(root)));

		Project projectRoot = treeStore.load();
		timesMainController.setProjects(projectRoot);
		timesMainController.setSpans(spansStore.load(projectRoot));

		primaryStage.setTitle(AppNameWindowTitle.fmt());

		primaryStage.addEventHandler(KeyEvent.KEY_PRESSED, ev -> {
			if (ev.getCode() == KeyCode.Q && ev.isControlDown())
				fireCloseRequest();
		});

		if (canShowTrayIcon(logo)) {
			log.info("Showing FXTrayIcon ...");
			new FXTrayIcon.Builder(primaryStage, logo).addTitleItem(true).addExitMenuItem(MenuItems.Exit.fmt()).show()
					.build();
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
	 * @return true iff we are on Windows and have a non-null logo
	 */
	private static boolean canShowTrayIcon(Image appLogo) {
		return System.getProperty("os.name").toLowerCase().contains("win") && appLogo != null;
	}

	@Override
	public void stop() {
		log.info("Stopping ...");
		try {
			treeStore.save(timesMainController.getProjects());
		} catch (IOException e) {
			log.error("Could not save projects", e);
		}
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
