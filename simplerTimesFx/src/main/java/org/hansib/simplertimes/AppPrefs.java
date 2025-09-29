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
package org.hansib.simplertimes;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class AppPrefs {

	private static final Logger log = LogManager.getLogger();

	private static AppPrefs instance;

	private final Prefs.App appPrefs;

	private AppPrefs() {
		appPrefs = load();
	}

	private static Prefs.App load() {
		Path prefsPath = DataPaths.atDefault().prefsPath();
		if (!prefsPath.toFile().isFile())
			return new Prefs.App();
		try {
			String yaml = Files.readString(prefsPath);
			ObjectMapper mapper = new ObjectMapper(new YAMLFactory());
			return mapper.readValue(yaml, Prefs.App.class);
		} catch (IOException e) {
			log.error(String.format("Encountered exception while trying to read preferences from '%s'", prefsPath), e);
			return new Prefs.App();
		}
	}

	public static AppPrefs get() {
		if (instance == null)
			instance = new AppPrefs();

		return instance;
	}

	public void save() {
		Path prefsPath = DataPaths.atDefault().prefsPath();
		ObjectMapper mapper = new ObjectMapper(new YAMLFactory());
		try {
			String yaml = mapper.writeValueAsString(appPrefs);
			Files.writeString(prefsPath, yaml);
		} catch (IOException e) {
			log.error("Encountered exception while trying to write preferences", e);
		}
	}

	public Prefs.Disclaimer disclaimer() {
		return appPrefs.disclaimer;
	}
}
