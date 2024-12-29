/*-
 * GPL for SimplerTimes - https://github.com/hansi-b/SimplerTimes
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
import java.util.function.Consumer;
import java.util.function.Supplier;

import javafx.application.Platform;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.ReadOnlyBooleanProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.scene.control.Button;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.controlsfx.control.SearchableComboBox;
import org.hansib.simplertimes.fx.data.FxProject;
import org.hansib.simplertimes.fx.data.ObservableData;
import org.hansib.simplertimes.spans.Span;
import org.hansib.simplertimes.times.Interval;
import org.hansib.simplertimes.times.IntervalTicker;
import org.hansib.simplertimes.times.Utils;
import org.hansib.sundries.fx.ButtonBuilder;
import org.hansib.sundries.testing.VisibleForTesting;

class SpanRecorder {

	private static final Logger log = LogManager.getLogger();

	private static final Duration minimumSpanDuration = Duration.ofSeconds(1);

	private final SearchableComboBox<FxProject> projectSelection;

	private final IntervalTicker intervalTicker;
	private final Supplier<ObservableData> lazySpanReceiver;

	private final BooleanProperty isRecording = new SimpleBooleanProperty(false);

	SpanRecorder(SearchableComboBox<FxProject> projectSelection, Button startButton, Button stopButton,
			Consumer<Duration> elapsedTimeDisplay, Supplier<ObservableData> lazySpanReceiver) {

		this(projectSelection, startButton, stopButton, elapsedTimeDisplay, lazySpanReceiver,
				new IntervalTicker(i -> elapsedTimeDisplay.accept(Span.effectiveDuration(i.start(), i.end()))));
	}

	@VisibleForTesting
	SpanRecorder(SearchableComboBox<FxProject> projectSelection, Button startButton, Button stopButton,
			Consumer<Duration> elapsedTimeDisplay, Supplier<ObservableData> lazySpanReceiver,
			IntervalTicker intervalTicker) {

		this.projectSelection = projectSelection;

		this.intervalTicker = intervalTicker;
		this.lazySpanReceiver = lazySpanReceiver;

		new ButtonBuilder(startButton) //
				.graphic(Icons.start()).onAction(a -> startRecording()).disabled().build();
		new ButtonBuilder(stopButton) //
				.graphic(Icons.stop()).onAction(a -> stopRecording()).disabled().build();

		startButton.disableProperty()
				.bind(projectSelection.getSelectionModel().selectedItemProperty().isNull().or(isRecording));
		stopButton.disableProperty().bind(isRecording.not());
		projectSelection.disableProperty().bind(isRecording);

		FxConverters.setComboBoxProjectConverter(projectSelection);
		projectSelection.showingProperty()
				.addListener((observable, oldValue, newValue) -> elapsedTimeDisplay.accept(Duration.ZERO));
		projectSelection.addEventHandler(KeyEvent.KEY_RELEASED, event -> {
			if (event.getCode() == KeyCode.ENTER) {
				Platform.runLater(startButton::requestFocus);
			}
		});
	}

	ReadOnlyBooleanProperty isRecordingProperty() {
		return isRecording;
	}

	void startRecording(FxProject project) {

		if (intervalTicker.isStarted()) {
			log.info("Implicitly stopping {}", projectSelection.getValue());
			stopRecording();
		}

		log.info("Starting {}", project);
		projectSelection.setValue(project);
		startRecording();
	}

	private void startRecording() {
		isRecording.set(true);
		intervalTicker.start();
	}

	void stopRecording() {
		isRecording.set(false);
		intervalTicker.stopAndGet();

		FxProject project = projectSelection.getValue();
		Interval t = intervalTicker.lastUpdate();

		if (Duration.between(t.start(), t.end()).compareTo(minimumSpanDuration) > 0) {
			log.info("Registering {} during {}", project, t);
			lazySpanReceiver.get().addSpan(project, t.start(), t.end());
		} else {
			log.info("Ignoring interval {} - {} (is smaller than {})", t::start, t::end,
					() -> Utils.toHmsString(minimumSpanDuration));
		}

		projectSelection.requestFocus();
	}
}