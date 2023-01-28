package org.hansib.simplertimesfx;

import java.time.Duration;
import java.util.Collection;
import java.util.Set;
import java.util.function.Consumer;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hansib.simplertimes.projects.Project;
import org.hansib.simplertimes.spans.Span;
import org.hansib.simplertimes.times.DurationTicker;
import org.hansib.simplertimes.times.Interval;
import org.hansib.sundries.fx.ButtonDecorator;
import org.hansib.sundries.fx.Converters;
import org.hansib.sundries.fx.FilteringComboBox;

import javafx.collections.ObservableList;
import javafx.scene.control.Button;
import javafx.scene.control.ComboBox;

class SpanRecorder {

	private static final Logger log = LogManager.getLogger();

	private final ComboBox<Project> projectSelection;
	private final ObservableList<Project> originalList;

	private final Button startButton;
	private final Button stopButton;

	private final DurationTicker durationTicker;
	private final Consumer<Span> spanReceiver;

	SpanRecorder(ComboBox<Project> projectSelection, Button startButton, Button stopButton,
			Consumer<Duration> tickReceiver, Consumer<Span> spanReceiver) {
		this.projectSelection = projectSelection;
		this.originalList = projectSelection.getItems();

		this.startButton = startButton;
		this.stopButton = stopButton;

		this.spanReceiver = spanReceiver;
		this.durationTicker = new DurationTicker(tickReceiver);

		new ButtonDecorator(startButton).graphic(Icons.start()).onAction(a -> startTiming()).enabled();
		new ButtonDecorator(stopButton).graphic(Icons.stop()).onAction(a -> stopTiming()).disabled();

		projectSelection.setConverter( //
				new Converters().stringConverter( //
						proj -> proj == null ? "" : fullName(proj), //
						projName -> projName == null || projName.isBlank() ? null
								: projectSelection.getSelectionModel().getSelectedItem()));

		new FilteringComboBox<>(projectSelection) //
				.withLcWordsFilterBuilder(words -> p -> matches(p, words)) //
				.withActionOnEnter(this::startInterval) //
				.build();
	}

	void updateProjects(Collection<Project> newProjects) {
		originalList.setAll(newProjects);
	}

	private void startInterval() {
		startButton.requestFocus();
		startButton.fire();
	}

	private void startTiming() {
		startButton.setDisable(true);
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
	}

	private static String fullName(Project p) {

		/*
		 * other options: · • › » ▹ ▷ | – #
		 */
		return String.join(" › ", p.nameWords());
	}

	private static boolean matches(Project p, Set<String> targetLcWords) {
		if (targetLcWords.isEmpty())
			return true;
		return targetLcWords.stream()
				.allMatch(tw -> p.nameWords().stream().map(String::toLowerCase).anyMatch(pw -> pw.contains(tw)));
	}
}