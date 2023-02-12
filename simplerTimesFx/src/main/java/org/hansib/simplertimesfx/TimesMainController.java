package org.hansib.simplertimesfx;

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
