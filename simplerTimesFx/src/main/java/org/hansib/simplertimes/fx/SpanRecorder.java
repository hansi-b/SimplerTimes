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

import java.time.Duration;
import java.util.function.Consumer;
import java.util.function.Supplier;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.controlsfx.control.SearchableComboBox;
import org.hansib.simplertimes.times.DurationTicker;
import org.hansib.simplertimes.times.Interval;
import org.hansib.simplertimes.times.Utils;
import org.hansib.sundries.fx.ButtonBuilder;
import org.hansib.sundries.fx.Converters;

import javafx.application.Platform;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.ReadOnlyBooleanProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.scene.control.Button;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;

class SpanRecorder {

	private static final Duration minimumSpanDuration = Duration.ofSeconds(1);

	private static final Logger log = LogManager.getLogger();

	private final SearchableComboBox<FxProject> projectSelection;

	private final DurationTicker durationTicker;
	private final Supplier<ObservableData> lazySpanReceiver;

	private final BooleanProperty isRecording = new SimpleBooleanProperty(false);

	SpanRecorder(SearchableComboBox<FxProject> projectSelection, Button startButton, Button stopButton,
			Consumer<Duration> tickReceiver, Supplier<ObservableData> lazySpanReceiver) {

		this.projectSelection = projectSelection;

		this.durationTicker = new DurationTicker(tickReceiver);
		this.lazySpanReceiver = lazySpanReceiver;

		new ButtonBuilder(startButton) //
				.graphic(Icons.start()).onAction(a -> startRecording()).disabled().build();
		new ButtonBuilder(stopButton) //
				.graphic(Icons.stop()).onAction(a -> stopRecording()).disabled().build();

		startButton.disableProperty()
				.bind(projectSelection.getSelectionModel().selectedItemProperty().isNull().or(isRecording));
		stopButton.disableProperty().bind(isRecording.not());
		projectSelection.disableProperty().bind(isRecording);

		projectSelection.setConverter( //
				new Converters().stringConverter( //
						proj -> proj == null ? "" : proj.fullName(), //
						projName -> projName == null || projName.isBlank() ? null
								: projectSelection.getSelectionModel().getSelectedItem()));
		projectSelection.showingProperty()
				.addListener((observable, oldValue, newValue) -> tickReceiver.accept(Duration.ZERO));
		projectSelection.addEventHandler(KeyEvent.KEY_RELEASED, event -> {
			if (event.getCode() == KeyCode.ENTER) {
				Platform.runLater(startButton::requestFocus);
			}
		});
	}

	ReadOnlyBooleanProperty isRecordingProperty() {
		return isRecording;
	}

	private void startRecording() {
		isRecording.set(true);
		durationTicker.start();
	}

	private void stopRecording() {
		isRecording.set(false);

		Interval t = durationTicker.stopAndGet();
		FxProject project = projectSelection.getValue();

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