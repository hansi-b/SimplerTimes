package org.hansib.simplertimesfx;

import java.time.Duration;
import java.util.Set;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hansib.simplertimes.projects.Project;
import org.hansib.simplertimes.spans.Span;
import org.hansib.simplertimes.spans.SpansCollection;
import org.hansib.simplertimes.times.DurationTicker;
import org.hansib.simplertimes.times.Interval;
import org.hansib.simplertimesfx.tree.TreeViewWindow;
import org.hansib.sundries.fx.Converters;
import org.hansib.sundries.fx.FilteringComboBox;
import org.hansib.sundries.fx.FxResourceLoader;

import javafx.application.Platform;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.ComboBox;
import javafx.scene.control.Label;
import javafx.stage.Stage;

public class TimesMainController {

	private static final Logger log = LogManager.getLogger();

	@FXML
	private ComboBox<Project> projectSelection;
	private ObservableList<Project> projectSelectionItems;

	@FXML
	private Button startButton;

	@FXML
	private Button stopButton;

	@FXML
	private Label elapsedTime;

	private DurationTicker timerDisplay;

	@FXML
	private Button editTreeButton;

	@FXML
	private Button showSpansButton;

	private SpansTableController spansTableController;
	private Stage spansStage;

	private SpansCollection spans;
	private Project projectTree;

	@FXML
	void initialize() {
		initializeTiming();

		spansTableController = loadSpansTableController();

		showSpansButton.setGraphic(Icons.showSpans());
		showSpansButton.setOnAction(event -> showSpansTable());

		editTreeButton.setGraphic(Icons.editTree());
		editTreeButton.setOnAction(event -> new TreeViewWindow(getProjects())
				.withCloseHandler(this::updateProjectSelectionItems).openTreeViewWindow(editTreeButton));

		projectSelectionItems = projectSelection.getItems();

		projectSelection.setConverter( //
				new Converters().stringConverter( //
						proj -> proj == null ? "" : fullName(proj), //
						projName -> projName == null || projName.isBlank() || projectTree == null ? null
								: projectSelection.getSelectionModel().getSelectedItem()));

		new FilteringComboBox<>(projectSelection) //
				.withLcWordsFilterBuilder(words -> p -> matches(p, words)) //
				.withActionOnEnter(this::startInterval) //
				.build();
	}

	void initializeTiming() {
		timerDisplay = new DurationTicker(this::updateTime);

		startButton.setGraphic(Icons.start());
		stopButton.setGraphic(Icons.stop());

		startButton.setDisable(false);
		stopButton.setDisable(true);

		startButton.setOnAction(a -> startTiming());
		stopButton.setOnAction(a -> stopTiming());

		updateTime(Duration.ZERO);
	}

	void startInterval() {
		startButton.requestFocus();
		startButton.fire();
	}

	private void startTiming() {
		startButton.setDisable(true);
		stopButton.setDisable(false);
		timerDisplay.start();
	}

	private void stopTiming() {
		stopButton.setDisable(true);
		startButton.setDisable(false);

		handleInterval(timerDisplay.stopAndGet());
	}

	private void updateTime(Duration duration) {
		Platform.runLater(() -> elapsedTime.setText(
				String.format("%d:%02d:%02d", duration.toHours(), duration.toMinutesPart(), duration.toSecondsPart())));
	}

	private void handleInterval(Interval t) {
		Project project = projectSelection.getValue();
		Project selectedItem = projectSelection.getSelectionModel().getSelectedItem();

		log.info("Got interval: {} {} {}", selectedItem, project, t);

		try {
			spans.add(new Span(project, t.start(), t.end()));
		} catch (IllegalArgumentException ex) {
			log.info("Ignoring invalid span: {}", ex.getMessage());
		}
	}

	private SpansTableController loadSpansTableController() {
		spansStage = new Stage();
		return new FxResourceLoader().loadFxmlToStage("spansTable.fxml", spansStage);
	}

	private void showSpansTable() {
		spansTableController.setSpans(spans);
		spansStage.show();
	}

	private void updateProjectSelectionItems() {
		if (projectTree == null)
			return;
		projectSelectionItems.setAll(projectTree.dfStream().filter(p -> p.name() != null).toList());
	}

	private static String fullName(Project p) {

		/*
		 * other options: · • › » ▹ ▷ | – #
		 */
		return String.join(" › ", p.nameWords());
	}

	private boolean matches(Project p, Set<String> targetLcWords) {
		if (targetLcWords.isEmpty())
			return true;
		return targetLcWords.stream()
				.allMatch(tw -> p.nameWords().stream().map(String::toLowerCase).anyMatch(pw -> pw.contains(tw)));
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
