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

public class ProjectStore {

	private static final Logger log = LogManager.getLogger();

	private final Path projectsPath;
	private final String newProjectName;

	/**
	 * @param newProjectName the name used for the first placeholder project if we
	 *                       don't have or cannot load an existing project tree
	 */
	public ProjectStore(Path projectsPath, String newProjectName) {
		this.projectsPath = projectsPath;
		this.newProjectName = newProjectName;
	}

	public Project load() {
		if (!projectsPath.toFile().isFile())
			return emptyTree();
		try {
			return new ProjectYamlConverter().fromYaml(Files.readString(projectsPath));
		} catch (IOException e) {
			log.error(String.format("Encountered exception while trying to read projects from '%s'", projectsPath), e);
			return emptyTree();
		}
	}

	private Project emptyTree() {
		Project root = Project.root();
		root.add(newProjectName);
		return root;
	}

	public void save(Project tree) throws IOException {
		Files.writeString(projectsPath, new ProjectYamlConverter().toYaml(tree));
	}
}
