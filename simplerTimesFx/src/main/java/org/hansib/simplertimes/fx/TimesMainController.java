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
package org.hansib.simplertimes.fx;

import static java.util.stream.Collectors.toCollection;

import java.time.Duration;
import java.util.ArrayList;

import org.controlsfx.control.SearchableComboBox;
import org.hansib.simplertimes.projects.Project;
import org.hansib.simplertimes.spans.Span;
import org.hansib.simplertimes.spans.SpansCollection;
import org.hansib.simplertimes.times.Utils;

import javafx.application.Platform;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.layout.Pane;

public class TimesMainController {

	@FXML
	private Pane topLevelPane;

	@FXML
	private SearchableComboBox<Project> projectSelection;

	@FXML
	private Button startButton;

	@FXML
	private Button stopButton;

	@FXML
	private Label elapsedTime;

	@FXML
	private Button editTreeButton;

	@FXML
	private Button showSpansButton;

	private final ObservableList<FxSpan> spans = FXCollections.observableArrayList();

	private Project projectTree;
	private final ObservableList<Project> projectList = FXCollections.observableArrayList();

	@FXML
	void initialize() {

		setElapsedTime(Duration.ZERO);
		projectSelection.setItems(projectList);

		new SpansDisplay(showSpansButton, () -> spans, () -> projectList);
		new TreeDisplay(editTreeButton, this::getProjectTree, this::updateProjectList);

		SpanRecorder spanRecorder = new SpanRecorder(projectSelection, startButton, stopButton, this::setElapsedTime,
				this::addSpan);
		editTreeButton.disableProperty().bind(spanRecorder.isRecordingProperty());
	}

	private void updateProjectList() {
		if (projectTree == null)
			return;
		projectList.setAll(projectTree.dfStream().filter(p -> p.name() != null).toList());
	}

	private void setElapsedTime(Duration duration) {
		Platform.runLater(() -> elapsedTime.setText(Utils.toHmsString(duration)));
	}

	void setProjects(Project projectTree) {
		this.projectTree = projectTree;
		updateProjectList();
	}

	Project getProjectTree() {
		return projectTree;
	}

	void setSpans(SpansCollection spansCollection) {
		spans.setAll(spansCollection.stream().map(FxSpan::new).collect(toCollection(() -> new ArrayList<>())));
	}

	private void addSpan(Span span) {
		spans.add(new FxSpan(span));
	}

	SpansCollection getSpans() {
		SpansCollection spansCollection = new SpansCollection();
		spans.forEach(r -> spansCollection.add(r.toSpan()));
		return spansCollection;
	}

}
