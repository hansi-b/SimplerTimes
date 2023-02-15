/**
 * Abakus - https://github.com/hansi-b/SimplerTimesFx
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
package org.hansib.simplertimesfx;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hansib.simplertimesfx.l10n.MenuItems;

import com.dustinredmond.fxtrayicon.FXTrayIcon;

import javafx.scene.image.Image;
import javafx.stage.Stage;

class SimplerTimesTrayIcon {

	/**
	 * from https://github.com/dustinkredmond/FXTrayIcon/issues/67#issue-1445289256
	 */
	private static class ScaledFXTrayIcon extends FXTrayIcon {
		public ScaledFXTrayIcon(Stage parentStage, Image iconImage, int width, int height) {
			super(parentStage, iconImage, width, height);
			super.trayIcon.setImageAutoSize(true);
		}
	}

	private static final Logger log = LogManager.getLogger();
	private Image logo;
	private Stage primaryStage;

	SimplerTimesTrayIcon(Stage primaryStage, Image logo) {
		this.primaryStage = primaryStage;
		this.logo = logo;
	}

	void show() {
		log.info("Showing FXTrayIcon ...");
		FXTrayIcon trayIcon = new ScaledFXTrayIcon(primaryStage, logo, 128, 128);
		trayIcon.addTitleItem(true);
		trayIcon.addExitItem(MenuItems.Exit.fmt());
		trayIcon.show();
	}
}