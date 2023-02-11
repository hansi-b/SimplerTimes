package org.hansib.simplertimesfx;

import java.time.Duration;
import java.util.Collection;
import java.util.function.Consumer;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.controlsfx.control.SearchableComboBox;
import org.hansib.simplertimes.projects.Project;
import org.hansib.simplertimes.spans.Span;
import org.hansib.simplertimes.times.DurationTicker;
import org.hansib.simplertimes.times.Interval;
import org.hansib.sundries.fx.ButtonDecorator;
import org.hansib.sundries.fx.Converters;

import javafx.application.Platform;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.ReadOnlyBooleanProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.collections.ObservableList;
import javafx.scene.control.Button;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;

class SpanRecorder {

	private static final Logger log = LogManager.getLogger();

	private final SearchableComboBox<Project> projectSelection;
	private final ObservableList<Project> originalList;

	private final DurationTicker durationTicker;
	private final Consumer<Span> spanReceiver;

	private final BooleanProperty isRecording = new SimpleBooleanProperty(false);

	SpanRecorder(SearchableComboBox<Project> projectSelection, Button startButton, Button stopButton,
			Consumer<Duration> tickReceiver, Consumer<Span> spanReceiver) {

		this.projectSelection = projectSelection;
		this.originalList = projectSelection.getItems();

		this.spanReceiver = spanReceiver;
		this.durationTicker = new DurationTicker(tickReceiver);

		new ButtonDecorator(startButton).graphic(Icons.start()).onAction(a -> startRecording()).disabled();
		new ButtonDecorator(stopButton).graphic(Icons.stop()).onAction(a -> stopRecording()).disabled();

		startButton.disableProperty()
				.bind(projectSelection.getSelectionModel().selectedItemProperty().isNull().or(isRecording));
		stopButton.disableProperty().bind(isRecording.not());
		projectSelection.disableProperty().bind(isRecording);

		projectSelection.setConverter( //
				new Converters().stringConverter( //
						proj -> proj == null ? "" : fullName(proj), //
						projName -> projName == null || projName.isBlank() ? null
								: projectSelection.getSelectionModel().getSelectedItem()));

		projectSelection.addEventHandler(KeyEvent.KEY_RELEASED, event -> {
			if (event.getCode() == KeyCode.ENTER) {
				Platform.runLater(startButton::requestFocus);
			}
		});
	}

	ReadOnlyBooleanProperty isRecordingProperty() {
		return isRecording;
	}

	void updateProjects(Collection<Project> newProjects) {
		originalList.setAll(newProjects);
	}

	private void startRecording() {
		isRecording.set(true);
		durationTicker.start();
	}

	private void stopRecording() {
		isRecording.set(false);

		Interval t = durationTicker.stopAndGet();
		Project project = projectSelection.getValue();

		Span span = new Span(project, t.start(), t.end());
		log.info("Got {}", span);

		spanReceiver.accept(span);
		projectSelection.requestFocus();
	}

	private static String fullName(Project p) {

		/*
		 * other options: · • › » ▹ ▷ | – #
		 */
		return String.join(" › ", p.nameWords());
	}
}