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
import javafx.stage.Stage;

import org.controlsfx.control.SearchableComboBox;
import org.hansib.simplertimes.fx.data.FxProject;
import org.hansib.simplertimes.fx.data.ObservableData;
import org.hansib.simplertimes.fx.l10n.General;
import org.hansib.simplertimes.fx.tree.TreeViewWindow;
import org.hansib.simplertimes.prefs.AppPrefs;
import org.hansib.simplertimes.prefs.Prefs;
import org.hansib.simplertimes.prefs.Prefs.Windows;
import org.hansib.simplertimes.times.Utils;
import org.hansib.sundries.fx.ButtonBuilder;
import org.hansib.sundries.fx.ControllerLoader;
import org.hansib.sundries.fx.StageData;
import org.hansib.sundries.fx.StageToggle;

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

	private final ExitManager exitManager;
	private final ObservableData data;

	private SpanRecorder spanRecorder;

	TimesMainController(ObservableData data, ExitManager exitManager) {
		this.data = data;
		this.exitManager = exitManager;
	}

	@FXML
	void initialize() {
		projectSelection.setItems(data.projects());

		Windows windowPrefs = AppPrefs.get().windowPositions.current();
		buildSpansDisplay(showSpansButton, windowPrefs);
		buildTreeDisplay(editTreeButton, windowPrefs);

		setElapsedTime(Duration.ZERO);
		spanRecorder = new SpanRecorder(projectSelection, startButton, stopButton, this::setElapsedTime, data);
		editTreeButton.disableProperty().bind(spanRecorder.isRecordingProperty());
	}

	private void setElapsedTime(Duration duration) {
		Platform.runLater(() -> elapsedTime.setText(Utils.toHmsString(duration)));
	}

	SpanRecorder getRecorder() {
		return spanRecorder;
	}

	private void buildSpansDisplay(Button showSpansButton, Prefs.Windows windowPrefs) {

		StageToggle stageToggle = new StageToggle(() -> initSpansStage(windowPrefs), windowPrefs.spans);
		new ButtonBuilder(showSpansButton) //
				.graphic(Icons.showSpans()).onAction(event -> stageToggle.toggle()) //
				.build();
	}

	private Stage initSpansStage(Prefs.Windows windowPrefs) {
		Stage spansStage = new Stage();
		spansStage.setTitle(General.SpansWindowTitle.fmt());

		SpansInfoController spansInfoController = ControllerLoader.<SpansInfoController>of("spansInfo.fxml")
				.withTargetStage(spansStage).load();
		spansInfoController.setData(data);

		new Resources().loadLogo(logo -> spansStage.getIcons().add(logo));
		exitManager.addPreExitAction(() -> windowPrefs.spans = StageData.of(spansStage));
		return spansStage;
	}

	private void buildTreeDisplay(Button editTreeButton, Prefs.Windows windowPrefs) {
		StageToggle stageToggle = new StageToggle(this::initTreeViewStage, windowPrefs.projects);
		exitManager.addPreExitAction(() -> windowPrefs.projects = StageData.of(stageToggle.getStage()));
		new ButtonBuilder(editTreeButton).graphic(Icons.editTree()).onAction(event -> stageToggle.toggle()).build();
	}

	private Stage initTreeViewStage() {
		TreeViewWindow<FxProject> treeViewWindow = new TreeViewWindow<>(data.fxProjectTree(), data::updateProjectList);
		treeViewWindow.setPreRemovalChecker(data.fxProjectRemovalCallback());
		return treeViewWindow.initStage();
	}
}
