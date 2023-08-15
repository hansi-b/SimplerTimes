package org.hansib.simplertimes.fx;

import java.time.ZonedDateTime;
import java.util.List;

import org.hansib.simplertimes.DataStore;
import org.hansib.simplertimes.projects.Project;
import org.hansib.simplertimes.spans.Span;
import org.hansib.simplertimes.spans.SpansCollection;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

public class ObservableData {

	private final FxProject fxProjectTree;
	private final ObservableList<Project> projectList = FXCollections.observableArrayList();

	private final ObservableList<FxSpan> spans = FXCollections.observableArrayList();

	private ObservableData(FxProject fxProjectTree, List<FxSpan> spans) {
		this.fxProjectTree = fxProjectTree;
		updateProjectList();
		this.spans.setAll(spans);
	}

	public void updateProjectList() {
		projectList.setAll(fxProjectTree.project().dfStream().filter(p -> p.name() != null).toList());
	}

	static ObservableData load(DataStore dataStore) {
		Project projectTree = dataStore.loadProjectTree();
		return new ObservableData(FxProject.root(projectTree),
				dataStore.loadSpans(projectTree).stream().map(FxSpan::new).toList());
	}

	void store(DataStore dataStore) {
		SpansCollection spansCollection = new SpansCollection();
		spans.forEach(r -> spansCollection.add(r.toSpan()));
		dataStore.save(fxProjectTree.project(), spansCollection);
	}

	public FxProject fxProjectTree() {
		return fxProjectTree;
	}

	ObservableList<Project> projects() {
		return projectList;
	}

	ObservableList<FxSpan> spans() {
		return spans;
	}

	public void addSpan(Project project, ZonedDateTime start, ZonedDateTime end) {
		spans.add(new FxSpan(new Span(project, start, end)));
	}
}