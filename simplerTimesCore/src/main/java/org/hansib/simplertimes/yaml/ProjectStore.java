package org.hansib.simplertimes.yaml;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hansib.simplertimes.AppData;
import org.hansib.simplertimes.projects.Project;

public class ProjectStore {

	private static final Logger log = LogManager.getLogger();

	private final Path projectsPath;

	public ProjectStore() {
		this.projectsPath = new AppData().dataPath("projects.yml");
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

	private static Project emptyTree() {
		Project root = Project.root();
		root.add("New Project");
		return root;
	}

	public void save(Project tree) throws IOException {
		Files.writeString(projectsPath, new ProjectYamlConverter().toYaml(tree));
	}
}
