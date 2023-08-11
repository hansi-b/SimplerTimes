package org.hansib.simplertimes.fx;

import java.util.List;

import org.hansib.simplertimes.DataStore;
import org.hansib.simplertimes.projects.Project;
import org.hansib.simplertimes.spans.Span;
import org.hansib.simplertimes.spans.SpansCollection;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

public class ObservableData {

	private final Project projectTree;
	private final ObservableList<Project> projectList = FXCollections.observableArrayList();

	private final ObservableList<FxSpan> spans = FXCollections.observableArrayList();

	private ObservableData(Project projectTree, List<FxSpan> spans) {
		this.projectTree = projectTree;
		updateProjectList();
		this.spans.setAll(spans);
	}

	public void updateProjectList() {
		projectList.setAll(projectTree.dfStream().filter(p -> p.name() != null).toList());
	}

	static ObservableData load(DataStore dataStore) {
		Project projectTree = dataStore.loadProjectTree();
		return new ObservableData(projectTree, dataStore.loadSpans(projectTree).stream().map(FxSpan::new).toList());
	}

	void store(DataStore dataStore) {
		SpansCollection spansCollection = new SpansCollection();
		spans.forEach(r -> spansCollection.add(r.toSpan()));
		dataStore.save(projectTree, spansCollection);
	}

	public Project projectTree() {
		return projectTree;
	}

	ObservableList<Project> projects() {
		return projectList;
	}

	ObservableList<FxSpan> spans() {
		return spans;
	}

	void addSpan(Span span) {
		spans.add(new FxSpan(span));
	}
}