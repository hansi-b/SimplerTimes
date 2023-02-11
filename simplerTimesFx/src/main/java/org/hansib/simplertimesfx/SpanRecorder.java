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
import javafx.beans.value.ChangeListener;
import javafx.collections.ObservableList;
import javafx.scene.control.Button;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;

class SpanRecorder {

	private static final Logger log = LogManager.getLogger();

	private final SearchableComboBox<Project> projectSelection;
	private final ObservableList<Project> originalList;

	private final Button startButton;
	private final Button stopButton;

	private final DurationTicker durationTicker;
	private final Consumer<Span> spanReceiver;

	SpanRecorder(SearchableComboBox<Project> projectSelection, Button startButton, Button stopButton,
			Consumer<Duration> tickReceiver, Consumer<Span> spanReceiver) {
		this.projectSelection = projectSelection;
		this.originalList = projectSelection.getItems();

		this.startButton = startButton;
		this.stopButton = stopButton;

		this.spanReceiver = spanReceiver;
		this.durationTicker = new DurationTicker(tickReceiver);

		new ButtonDecorator(startButton).graphic(Icons.start()).onAction(a -> startTiming()).disabled();
		new ButtonDecorator(stopButton).graphic(Icons.stop()).onAction(a -> stopTiming()).disabled();

		projectSelection.getSelectionModel().selectedItemProperty()
				.addListener((ChangeListener<? super Project>) (observable, oldValue, newValue) -> startButton
						.disableProperty().set(newValue == null));

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

	void updateProjects(Collection<Project> newProjects) {
		originalList.setAll(newProjects);
	}

	private void startTiming() {
		startButton.setDisable(true);
		projectSelection.setDisable(true);

		stopButton.setDisable(false);
		durationTicker.start();
	}

	private void stopTiming() {
		stopButton.setDisable(true);

		Interval t = durationTicker.stopAndGet();
		Project project = projectSelection.getValue();

		Span span = new Span(project, t.start(), t.end());
		log.info("Got {}", span);

		spanReceiver.accept(span);
		startButton.setDisable(false);
		projectSelection.setDisable(false);
		projectSelection.requestFocus();
	}

	private static String fullName(Project p) {

		/*
		 * other options: · • › » ▹ ▷ | – #
		 */
		return String.join(" › ", p.nameWords());
	}
}