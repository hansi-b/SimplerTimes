package org.hansib.simplertimes.fx;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hansib.simplertimes.fx.data.FxProject;
import org.hansib.simplertimes.fx.data.ObservableData;
import org.hansib.simplertimes.fx.l10n.MenuItems;

import com.dustinredmond.fxtrayicon.FXTrayIcon;

import javafx.beans.InvalidationListener;
import javafx.collections.ListChangeListener;
import javafx.scene.image.Image;
import javafx.stage.Stage;

class TrayIconMenu {

	private static final Logger log = LogManager.getLogger();

	private FXTrayIcon iconWithMenu;
	private ObservableData data;

	TrayIconMenu(ObservableData data) {
		this.data = data;
		data.projects().addListener((InvalidationListener) observable -> log.info("invalidated = {}", observable));
		data.projects().addListener((ListChangeListener<FxProject>) c -> log.info("onChanged = {}", c));
	}

	void build(Stage primaryStage, Image logo) {
		log.info("Showing FXTrayIcon ...");
		iconWithMenu = new FXTrayIcon.Builder(primaryStage, logo) //
				.addTitleItem(true) //
				.addExitMenuItem(MenuItems.Exit.fmt()) //
				.show().build();
	}
}