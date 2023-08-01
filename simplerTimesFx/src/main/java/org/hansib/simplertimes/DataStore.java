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