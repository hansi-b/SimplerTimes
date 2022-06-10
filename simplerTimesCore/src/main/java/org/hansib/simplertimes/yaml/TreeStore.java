package org.hansib.simplertimes.yaml;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hansib.simplertimes.AppData;
import org.hansib.simplertimes.tree.Project;
import org.hansib.simplertimes.tree.TreeNode;
import org.hansib.simplertimes.tree.TreeNodeYamlConverter;

public class TreeStore {

	private static final Logger log = LogManager.getLogger();

	private final Path projectsPath;

	public TreeStore() {
		this.projectsPath = new AppData().dataPath("projects.yml");
	}

	public TreeNode load() {
		if (!projectsPath.toFile().isFile())
			return emptyTree();
		try {
			return new TreeNodeYamlConverter().fromYaml(Files.readString(projectsPath));
		} catch (IOException e) {
			log.error(String.format("Encountered exception while trying to read projects from '%s'", projectsPath), e);
			return emptyTree();
		}
	}

	private static TreeNode emptyTree() {
		TreeNode root = TreeNode.root();
		root.add(new Project("New Project"));
		return root;
	}

	public void save(TreeNode tree) throws IOException {
		Files.writeString(projectsPath, new TreeNodeYamlConverter().toYaml(tree));
	}
}
