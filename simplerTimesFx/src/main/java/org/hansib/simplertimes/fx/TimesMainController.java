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

import javafx.application.Platform;
import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.layout.Pane;

import org.controlsfx.control.SearchableComboBox;
import org.hansib.simplertimes.fx.data.FxProject;
import org.hansib.simplertimes.fx.data.ObservableData;
import org.hansib.simplertimes.fx.tree.TreeDisplay;
import org.hansib.simplertimes.prefs.AppPrefs;
import org.hansib.simplertimes.times.Utils;

public class TimesMainController {

	@FXML
	private Pane topLevelPane;

	@FXML
	private SearchableComboBox<FxProject> projectSelection;

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

	private final ObservableData data;

	private SpanRecorder spanRecorder;

	TimesMainController(ObservableData data) {
		this.data = data;
	}

	@FXML
	void initialize() {
		projectSelection.setItems(data.projects());

		new SpansDisplay(showSpansButton, this::getData, ExitManager.get(), AppPrefs.get().windows);
		new TreeDisplay(editTreeButton, this::getData, ExitManager.get(), AppPrefs.get().windows);

		setElapsedTime(Duration.ZERO);
		spanRecorder = new SpanRecorder(projectSelection, startButton, stopButton, this::setElapsedTime, this::getData);
		editTreeButton.disableProperty().bind(spanRecorder.isRecordingProperty());
	}

	SpanRecorder getRecorder() {
		return spanRecorder;
	}

	private void setElapsedTime(Duration duration) {
		Platform.runLater(() -> elapsedTime.setText(Utils.toHmsString(duration)));
	}

	ObservableData getData() {
		return data;
	}
}
