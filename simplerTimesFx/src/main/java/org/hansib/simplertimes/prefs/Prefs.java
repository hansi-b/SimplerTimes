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

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hansib.simplertimes.DataPaths;
import org.hansib.sundries.fx.StageData;

public interface Prefs {

	enum AppVersion {
		v1
	}

	class AppPrefs {

		public static AppPrefs instance;

		private static final Logger log = LogManager.getLogger();

		public AppVersion version = AppVersion.v1;
		public Disclaimer disclaimer = new Disclaimer();
		public Windows windows = new Windows();

		public static AppPrefs get() {
			if (instance == null)
				instance = load();
			return instance;
		}

		private static AppPrefs load() {
			Path prefsPath = DataPaths.atDefault().prefsPath();
			if (!prefsPath.toFile().isFile())
				return new AppPrefs();
			try {
				String yaml = Files.readString(prefsPath);
				ObjectMapper mapper = new ObjectMapper(new YAMLFactory());
				mapper.setVisibility(mapper.getSerializationConfig().getDefaultVisibilityChecker()
					.withFieldVisibility(JsonAutoDetect.Visibility.ANY)
					.withGetterVisibility(JsonAutoDetect.Visibility.NONE)
					.withIsGetterVisibility(JsonAutoDetect.Visibility.NONE)
					.withSetterVisibility(JsonAutoDetect.Visibility.NONE)
					.withCreatorVisibility(JsonAutoDetect.Visibility.NONE));
				return mapper.readValue(yaml, AppPrefs.class);
			} catch (IOException e) {
				log.error(String.format("Encountered exception while trying to read preferences from '%s'", prefsPath),
					e);
				return new AppPrefs();
			}
		}

		public void save() {
			Path prefsPath = DataPaths.atDefault().prefsPath();
			ObjectMapper mapper = new ObjectMapper(new YAMLFactory());
			mapper.setVisibility(mapper.getSerializationConfig().getDefaultVisibilityChecker()
				.withFieldVisibility(JsonAutoDetect.Visibility.ANY).withGetterVisibility(JsonAutoDetect.Visibility.NONE)
				.withIsGetterVisibility(JsonAutoDetect.Visibility.NONE)
				.withSetterVisibility(JsonAutoDetect.Visibility.NONE)
				.withCreatorVisibility(JsonAutoDetect.Visibility.NONE));
			try {
				String yaml = mapper.writeValueAsString(this);
				Files.writeString(prefsPath, yaml);
			} catch (IOException e) {
				log.error("Encountered exception while trying to write preferences", e);
			}
		}

	}

	class Disclaimer {
		public boolean isAccepted = false;
	}

	class Windows {
		public StageData mainWindow = StageData.NONE;
	}
}