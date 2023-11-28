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

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hansib.simplertimes.fx.data.ObservableData;
import org.hansib.simplertimes.fx.l10n.MenuItems;

import com.dustinredmond.fxtrayicon.FXTrayIcon;

import javafx.stage.Stage;

class TrayIconMenu {

	private static final Logger log = LogManager.getLogger();

	private FXTrayIcon iconWithMenu;
	private ObservableData data;

	TrayIconMenu() {// ObservableData data) {
//		this.data = data;
//		data.projects().addListener((InvalidationListener) observable -> log.info("invalidated = {}", observable));
//		data.projects().addListener((ListChangeListener<FxProject>) c -> log.info("onChanged = {}", c));
	}

	static TrayIconMenu create(Stage primaryStage) {
		log.info("Showing FXTrayIcon ...");
		TrayIconMenu tim = new TrayIconMenu();
		tim.iconWithMenu = new FXTrayIcon.Builder(primaryStage, new Resources().loadLogo()) //
				.addTitleItem(true) //
				.addExitMenuItem(MenuItems.Exit.fmt()) //
				.show().build();
		return tim;
	}
}