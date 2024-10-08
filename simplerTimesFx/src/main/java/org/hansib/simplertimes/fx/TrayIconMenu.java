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
import java.awt.GraphicsEnvironment;
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
import javafx.collections.ListChangeListener;
import javafx.stage.Stage;

class TrayIconMenu {

	private static final Logger log = LogManager.getLogger();

	TrayIconMenu(ObservableData data) {
	}

	static void create(Stage primaryStage, ObservableData data, SpanRecorder spanRecorder) {
		if (!SystemTray.isSupported() || GraphicsEnvironment.isHeadless()) {
			log.warn("Cannot resize logo (System tray not supported, or using headless graphics).");
			return;
		}

		data.projects().addListener((InvalidationListener) observable -> log.info("invalidated = {}", observable));
		data.projects().addListener((ListChangeListener<FxProject>) c -> log.info("onChanged = {}", c));

		log.info("Showing TrayIcon ...");
		Platform.setImplicitExit(false);
		TrayIcon trayIcon = new TrayIcon(new Resources().loadAwtLogo());

		final PopupMenu popup = new PopupMenu();
		final SystemTray tray = SystemTray.getSystemTray();

		MenuItem showItem = new MenuItem("SimplerTimes");
		showItem.addActionListener(e -> Platform.runLater(() -> {
			if (primaryStage.isIconified())
				primaryStage.setIconified(false);
			else
				primaryStage.show();
		}));

		MenuItem exitItem = new MenuItem("Exit");
		exitItem.addActionListener(e -> {
			tray.remove(trayIcon);
			Platform.exit();
		});
		MenuItem stopItem = new MenuItem("Stop");
		stopItem.addActionListener(e -> {
			Platform.runLater(spanRecorder::stopRecording);
			stopItem.setEnabled(false);
		});
		stopItem.setEnabled(false);

		popup.add(showItem);
		popup.addSeparator();

		data.fxProjectTree().children().map(project -> createMenu(project, spanRecorder, stopItem)).forEach(popup::add);

		popup.addSeparator();
		popup.add(stopItem);
		popup.add(exitItem);

		trayIcon.setPopupMenu(popup);
		try {
			tray.add(trayIcon);
		} catch (AWTException e) {
			log.error("Could not add tray icon", e);
		}
	}

	/*
	 * Create menu items of all project leaves in this project.
	 */
	private static MenuItem createMenu(FxProject project, SpanRecorder spanRecorder, MenuItem stopItem) {

		String projectName = project.text();
		if (!project.hasChildren()) {
			return createProjectMenuItem(projectName, project, spanRecorder, stopItem);
		}
		Menu projectMenu = new Menu(projectName);
		project.leafChildren().forEach(c -> projectMenu
				.add(createProjectMenuItem(c.formatName(project, " · "), project, spanRecorder, stopItem)));
		return projectMenu;
	}

	private static MenuItem createProjectMenuItem(String name, FxProject project, SpanRecorder spanRecorder,
			MenuItem stopItem) {
		MenuItem menuItem = new MenuItem(name);
		menuItem.addActionListener(e -> {
			Platform.runLater(() -> spanRecorder.startRecording(project));
			stopItem.setEnabled(true);
		});
		return menuItem;
	}

}