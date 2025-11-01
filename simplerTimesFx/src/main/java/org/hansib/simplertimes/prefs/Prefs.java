/**
 * GPL for SimplerTimes - https://github.com/hansi-b/SimplerTimes
 *
 * Copyright (C) 2022-2025 Hans Bering
 *
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with this program.  If not, see
 * <http://www.gnu.org/licenses/>.
 */
package org.hansib.simplertimes.prefs;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.StringJoiner;

import javafx.geometry.Rectangle2D;
import javafx.stage.Screen;

import org.hansib.sundries.fx.StageData;

public interface Prefs {

	enum AppVersion {
		v1
	}

	class Disclaimer {
		public boolean isAccepted = false;
	}

	class Windows {
		public StageData main = StageData.NONE;
		public StageData projects = StageData.NONE;
		public StageData spans = StageData.NONE;
	}

	/**
	 * Remember stage data by screen configuration to restore them only on known
	 * screens.
	 */
	class WindowsPositions {

		@SuppressWarnings("serial")
		private final Map<String, Windows> byScreenConfig = new LinkedHashMap<>() {
			private static final int CACHE_SIZE = 5;

			@Override
			protected boolean removeEldestEntry(final Map.Entry<String, Windows> eldest) {
				return size() > CACHE_SIZE;
			}
		};

		/**
		 * @return either stored positions for the current screen configuration or new
		 *         (undefined) positions
		 */
		public Windows current() {
			return byScreenConfig.computeIfAbsent(currentScreensCacheKey(), k -> new Windows());
		}

		/**
		 * @return a string representing the current screen configuration for use as a
		 *         cache key
		 */
		static String currentScreensCacheKey() {
			StringJoiner screenConfigStr = new StringJoiner("+");
			Screen.getScreens().forEach(s -> screenConfigStr.add(screenCacheKey(s)));
			return screenConfigStr.toString();
		}

		private static String screenCacheKey(Screen screen) {
			Rectangle2D bounds = screen.getVisualBounds();
			return String.format("@%s/%s:%sx%s", bounds.getMinX(), bounds.getMinY(), bounds.getWidth(),
					bounds.getHeight());
		}
	}
}