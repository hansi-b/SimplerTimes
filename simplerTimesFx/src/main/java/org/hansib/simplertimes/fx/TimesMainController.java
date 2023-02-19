/**
 * Abakus - https://github.com/hansi-b/SimplerTimesFx
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

import java.time.Duration;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.controlsfx.control.SearchableComboBox;
import org.hansib.simplertimes.projects.Project;
import org.hansib.simplertimes.spans.Span;
import org.hansib.simplertimes.spans.SpansCollection;

import javafx.application.Platform;
import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.Label;

public class TimesMainController {

	private static final Logger log = LogManager.getLogger();

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

	private SpansCollection spans;
	private Project projectTree;

	@FXML
	void initialize() {
		setElapsedTime(Duration.ZERO);

		spanRecorder = new SpanRecorder(projectSelection, startButton, stopButton, this::setElapsedTime, this::addSpan);

		new SpansTableDisplay(showSpansButton, this::getSpans);
		new TreeDisplay(editTreeButton, this::getProjects, this::updateProjectSelectionItems);
		editTreeButton.disableProperty().bind(spanRecorder.isRecordingProperty());
	}

	private void updateProjectSelectionItems() {
		if (projectTree == null)
			return;
		spanRecorder.updateProjects(projectTree.dfStream().filter(p -> p.name() != null).toList());
	}

	private void setElapsedTime(Duration duration) {
		int roundedSecs = duration.toSecondsPart() + (500 + duration.toMillisPart()) / 1000;
		Platform.runLater(() -> elapsedTime
				.setText(String.format("%d:%02d:%02d", duration.toHours(), duration.toMinutesPart(), roundedSecs)));
	}

	private void addSpan(Span span) {
		try {
			spans.add(span);
		} catch (IllegalArgumentException ex) {
			log.warn("Ignoring invalid span: {}", ex.getMessage());
		}
	}

	void setProjects(Project projects) {
		this.projectTree = projects;
		updateProjectSelectionItems();
	}

	Project getProjects() {
		return projectTree;
	}

	void setSpans(SpansCollection spans) {
		this.spans = spans;
	}

	SpansCollection getSpans() {
		return spans;
	}
}
