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