package org.hansib.simplertimes.fx;

import java.util.function.Consumer;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hansib.sundries.fx.FxResourceLoader;

import javafx.scene.image.Image;

public class Resources {

	private static final Logger log = LogManager.getLogger();

	private final FxResourceLoader fxLoader = new FxResourceLoader();

	public void loadLogo(Consumer<Image> logoHandler) {
		Image logo;
		try {
			logo = loadLogo();
		} catch (IllegalStateException ex) {
			log.warn("Could not load logo", ex);
			return;
		}
		logoHandler.accept(logo);
	}

	public Image loadLogo() {
		return fxLoader.resourceLoader().loadResourceStream("logo.png", Image::new);
	}
}