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

import java.net.URL;
import java.util.function.Consumer;

import javax.swing.ImageIcon;

import javafx.scene.image.Image;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hansib.sundries.ResourceLoader;

public class Resources {

	private static final Logger log = LogManager.getLogger();

	private final ResourceLoader loader = new ResourceLoader();

	public void loadLogo(Consumer<Image> logoHandler) {
		Image logo;
		try {
			logo = loader.loadResourceStream("logo.png", Image::new);
		} catch (IllegalStateException ex) {
			log.warn("Could not load logo", ex);
			return;
		}
		logoHandler.accept(logo);
	}

	public java.awt.Image loadAwtLogo() {
		URL imageURL = Resources.class.getClassLoader().getResource("logo.png");
		return new ImageIcon(imageURL, "SimplerTimes Logo").getImage()
			.getScaledInstance(16, 16, java.awt.Image.SCALE_SMOOTH);
	}
}