package org.hansib.simplertimesfx;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

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