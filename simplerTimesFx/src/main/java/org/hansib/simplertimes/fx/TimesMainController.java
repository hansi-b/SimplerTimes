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
import static javafx.collections.FXCollections.observableArrayList;

import java.time.Duration;
import java.util.ArrayList;

import org.controlsfx.control.SearchableComboBox;
import org.hansib.simplertimes.projects.Project;
import org.hansib.simplertimes.spans.Span;
import org.hansib.simplertimes.spans.SpansCollection;
import org.hansib.simplertimes.times.Utils;

import javafx.application.Platform;
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

	private SpanRecorder spanRecorder;

	private ObservableList<FxSpan> spans;
	private Project projectTree;

	@FXML
	void initialize() {

		setElapsedTime(Duration.ZERO);

		spanRecorder = new SpanRecorder(projectSelection, startButton, stopButton, this::setElapsedTime, this::addSpan);

		new SpansDisplay(showSpansButton, () -> spans);
		new TreeDisplay(editTreeButton, this::getProjects, this::updateProjectSelectionItems);
		editTreeButton.disableProperty().bind(spanRecorder.isRecordingProperty());
	}

	private void updateProjectSelectionItems() {
		if (projectTree == null)
			return;
		spanRecorder.updateProjects(projectTree.dfStream().filter(p -> p.name() != null).toList());
	}

	private void setElapsedTime(Duration duration) {
		Platform.runLater(() -> elapsedTime.setText(Utils.toHmsString(duration)));
	}

	private void addSpan(Span span) {
		spans.add(new FxSpan(span));
	}

	void setProjects(Project projects) {
		this.projectTree = projects;
		updateProjectSelectionItems();
	}

	Project getProjects() {
		return projectTree;
	}

	void setSpans(SpansCollection spansCollection) {
		this.spans = observableArrayList(
				spansCollection.stream().map(FxSpan::new).collect(toCollection(() -> new ArrayList<>())));
	}

	SpansCollection getSpans() {
		SpansCollection spansCollection = new SpansCollection();
		spans.forEach(r -> spansCollection.add(r.toSpan()));
		return spansCollection;
	}

}
