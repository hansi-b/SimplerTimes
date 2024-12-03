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
package org.hansib.simplertimes.yaml;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hansib.simplertimes.projects.Project;
import org.hansib.simplertimes.spans.SpansCollection;

public class SpansStore {

	private static final Logger log = LogManager.getLogger();

	private final Path spansPath;

	public SpansStore(Path spansPath) {
		this.spansPath = spansPath;
	}

	public SpansCollection load(Project root) {
		if (!spansPath.toFile().isFile())
			return new SpansCollection();
		try {
			return SpansYamlConverter.fromYaml(root, Files.readString(spansPath));
		} catch (IOException e) {
			log.error(String.format("Encountered exception while trying to read spans from '%s'", spansPath), e);
			return new SpansCollection();
		}
	}

	public void save(SpansCollection spans) throws IOException {
		Files.writeString(spansPath, SpansYamlConverter.toYaml(spans));
	}
}
