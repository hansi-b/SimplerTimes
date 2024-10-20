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

import java.awt.AWTException;
import java.awt.Menu;
import java.awt.MenuItem;
import java.awt.PopupMenu;
import java.awt.SystemTray;
import java.awt.TrayIcon;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hansib.simplertimes.fx.data.FxProject;
import org.hansib.simplertimes.fx.data.ObservableData;

import javafx.application.Platform;
import javafx.beans.InvalidationListener;
import javafx.stage.Stage;

class TrayIconMenu {

	private static final Logger log = LogManager.getLogger();

	private final ObservableData data;
	private final SpanRecorder spanRecorder;

	private final TrayIcon trayIcon;

	private final PopupMenu popup;

	private final MenuItem showItem;
	private final MenuItem stopItem;
	private final MenuItem exitItem;

	TrayIconMenu(ObservableData data, SpanRecorder spanRecorder, Stage primaryStage) {
		this.data = data;
		this.spanRecorder = spanRecorder;

		this.trayIcon = new TrayIcon(new Resources().loadAwtLogo());

		this.popup = new PopupMenu();
		this.showItem = createShowItem(primaryStage);
		this.exitItem = createExitItem();
		this.stopItem = createStopItem();
		this.stopItem.setEnabled(false);

		data.projects().addListener((InvalidationListener) observable -> fillPopup());
	}

	void show() {
		log.info("Showing TrayIcon ...");
		Platform.setImplicitExit(false);

		fillPopup();
		trayIcon.setPopupMenu(popup);
		try {
			SystemTray.getSystemTray().add(trayIcon);
		} catch (AWTException e) {
			log.error("Could not add tray icon", e);
		}
	}

	private void fillPopup() {
		log.info("Filling popup menu");

		popup.removeAll();
		popup.add(showItem);
		popup.addSeparator();

		data.fxProjectTree().children().map(project -> createMenu(project, stopItem)).forEach(popup::add);

		popup.addSeparator();
		popup.add(stopItem);
		popup.add(exitItem);
	}

	private static MenuItem createShowItem(Stage primaryStage) {
		MenuItem showItem = new MenuItem("SimplerTimes");
		showItem.addActionListener(e -> Platform.runLater(() -> {
			if (primaryStage.isIconified())
				primaryStage.setIconified(false);
			else
				primaryStage.show();
		}));
		return showItem;
	}

	private MenuItem createStopItem() {
		MenuItem item = new MenuItem("Stop");
		item.addActionListener(e -> {
			item.setEnabled(false);
			Platform.runLater(spanRecorder::stopRecording);
		});
		return item;
	}

	private MenuItem createExitItem() {
		MenuItem item = new MenuItem("Exit");
		item.addActionListener(e -> {
			SystemTray.getSystemTray().remove(trayIcon);
			Platform.exit();
		});
		return item;
	}

	/*
	 * Create menu items of all project leaves in this project.
	 */
	private MenuItem createMenu(FxProject project, MenuItem stopItem) {

		String projectName = project.text();
		if (!project.hasChildren()) {
			return createProjectMenuItem(projectName, project, stopItem);
		}
		Menu projectMenu = new Menu(projectName);
		project.leafChildren()
				.forEach(c -> projectMenu.add(createProjectMenuItem(c.formatName(project, " · "), project, stopItem)));
		return projectMenu;
	}

	private MenuItem createProjectMenuItem(String name, FxProject project, MenuItem stopItem) {
		MenuItem menuItem = new MenuItem(name);
		menuItem.addActionListener(e -> {
			Platform.runLater(() -> spanRecorder.startRecording(project));
			stopItem.setEnabled(true);
		});
		return menuItem;
	}

}