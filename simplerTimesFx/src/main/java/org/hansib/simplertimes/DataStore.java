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

import java.io.IOException;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hansib.simplertimes.projects.Project;
import org.hansib.simplertimes.spans.SpansCollection;
import org.hansib.simplertimes.yaml.ProjectStore;
import org.hansib.simplertimes.yaml.SpansStore;

public class DataStore {

	private static final Logger log = LogManager.getLogger();

	private final SpansStore spansStore;
	private final ProjectStore treeStore;

	public DataStore() {
		DataPaths appData = DataPaths.atDefault();

		spansStore = new SpansStore(appData.spansPath());
		treeStore = new ProjectStore(appData.projectsPath());
	}

	public Project loadProjectTree() {
		return treeStore.load();
	}

	public SpansCollection loadSpans(Project projects) {
		return spansStore.load(projects);
	}

	public void save(Project projects, SpansCollection spans) {
		try {
			treeStore.save(projects);
		} catch (IOException e) {
			log.error("Could not save projects", e);
		}
		try {
			spansStore.save(spans);
		} catch (IOException e) {
			log.error("Could not save spans", e);
		}
	}
}