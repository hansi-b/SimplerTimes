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
package org.hansib.simplertimes;

import java.nio.file.Files;
import java.nio.file.Path;

import org.hansib.sundries.Errors;

import net.harawata.appdirs.AppDirsFactory;

public class DataPaths {
	private final Path dataDir;

	private DataPaths(Path dataDir) {
		assertDataDir(dataDir);
		this.dataDir = dataDir;
	}

	private static void assertDataDir(Path dataDir) {
		if (Files.exists(dataDir)) {
			if (!Files.isDirectory(dataDir))
				throw Errors.illegalArg("Data directory argument '%s' exists, but is not a directory", dataDir);
		} else {
			dataDir.toFile().mkdirs();
		}
	}

	public static DataPaths atDefault() {
		return at(Path.of(AppDirsFactory.getInstance().getUserDataDir("SimplerTimes", "", "HansiB")));
	}

	public static DataPaths at(Path dataDir) {
		return new DataPaths(dataDir);

	}

	public Path projectsPath() {
		return dataPath("projects.yml");
	}

	public Path spansPath() {
		return dataPath("spans.yml");
	}

	private Path dataPath(String filename) {
		return dataDir.resolve(filename);
	}
}