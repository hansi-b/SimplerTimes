package org.hansib.simplertimes.fx.data;

import java.time.ZonedDateTime;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.hansib.simplertimes.DataStore;
import org.hansib.simplertimes.projects.Project;
import org.hansib.simplertimes.spans.Span;
import org.hansib.simplertimes.spans.SpansCollection;

import javafx.beans.Observable;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

public class ObservableData {

	private final FxProject fxProjectTree;
	private final ObservableList<FxProject> projectList = FXCollections
			.observableArrayList(p -> new Observable[] { p.name() });

	private final ObservableList<FxSpan> spans = FXCollections
			.observableArrayList(s -> new Observable[] { s.fxProject(), s.duration() });

	private ObservableData(FxProject fxProjectTree, List<FxSpan> spans) {
		this.fxProjectTree = fxProjectTree;
		updateProjectList();
		this.spans.setAll(spans);
	}

	public void updateProjectList() {
		projectList.setAll(fxProjectTree.flatList());
	}

	public static ObservableData load(DataStore dataStore) {
		Project projectTree = dataStore.loadProjectTree();
		FxProject fxProjectTree = FxProject.root(projectTree);

		Map<Project, FxProject> projectMap = fxProjectTree.flatList().stream()
				.collect(Collectors.toMap(p -> p.project(), p -> p));

		List<FxSpan> fxSpans = dataStore.loadSpans(projectTree).stream()
				.map(s -> new FxSpan(projectMap.get(s.project()), s.start(), s.end())).toList();

		return new ObservableData(fxProjectTree, fxSpans);
	}

	public void store(DataStore dataStore) {
		SpansCollection spansCollection = new SpansCollection();
		spans.forEach(s -> spansCollection.add(s.toSpan()));
		dataStore.save(fxProjectTree.project(), spansCollection);
	}

	public FxProject fxProjectTree() {
		return fxProjectTree;
	}

	public ObservableList<FxProject> projects() {
		return projectList;
	}

	public ObservableList<FxSpan> spans() {
		return spans;
	}

	public void addSpan(FxProject project, ZonedDateTime start, ZonedDateTime end) {
		// use the Span's truncation & precision:
		Span temp = new Span(project.project(), start, end);
		spans.add(new FxSpan(project, temp.start(), temp.end()));
	}

	public FxProjectRemovalCallback fxProjectRemovalCallback() {
		return new FxProjectRemovalCallback(spans);
	}
}