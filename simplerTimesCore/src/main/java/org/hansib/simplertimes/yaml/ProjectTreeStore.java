package org.hansib.simplertimes.yaml;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hansib.simplertimes.AppData;
import org.hansib.simplertimes.projects.Project;
import org.hansib.simplertimes.projects.ProjectTree;
import org.hansib.simplertimes.projects.ProjectTreeYamlConverter;

public class ProjectTreeStore {

	private static final Logger log = LogManager.getLogger();

	private final Path projectsPath;

	public ProjectTreeStore() {
		this.projectsPath = new AppData().dataPath("projects.yml");
	}

	public ProjectTree load() {
		if (!projectsPath.toFile().isFile())
			return emptyTree();
		try {
			return new ProjectTreeYamlConverter().fromYaml(Files.readString(projectsPath));
		} catch (IOException e) {
			log.error(String.format("Encountered exception while trying to read projects from '%s'", projectsPath), e);
			return emptyTree();
		}
	}

	private static ProjectTree emptyTree() {
		ProjectTree root = ProjectTree.root();
		root.add(new Project("New Project"));
		return root;
	}

	public void save(ProjectTree tree) throws IOException {
		Files.writeString(projectsPath, new ProjectTreeYamlConverter().toYaml(tree));
	}
}
