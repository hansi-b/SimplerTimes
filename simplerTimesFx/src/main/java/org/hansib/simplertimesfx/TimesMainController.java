package org.hansib.simplertimesfx;

import java.util.Collections;
import java.util.List;
import java.util.Set;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hansib.simplertimes.projects.Project;
import org.hansib.simplertimes.spans.Span;
import org.hansib.simplertimes.spans.SpansCollection;
import org.hansib.simplertimes.times.Interval;
import org.hansib.simplertimesfx.tree.TreeViewWindow;
import org.hansib.sundries.fx.FilteringComboBox;

import javafx.fxml.FXML;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.ComboBox;
import javafx.scene.control.TableView;
import javafx.scene.layout.HBox;
import javafx.stage.Stage;
import javafx.util.StringConverter;

public class TimesMainController {

	private static final Logger log = LogManager.getLogger();

	@FXML
	ComboBox<Project> projectSelection;

	@FXML
	ButtonsStripController buttonsStripController;

	@FXML
	Button editTreeButton;
	@FXML
	Button showSpansButton;

	private SpansCollection spans;
	private Project projectTree;

	@FXML
	void initialize() {
		buttonsStripController.setIntervalReceiver(this::handleInterval);

		editTreeButton.setGraphic(Icons.editTree());
		editTreeButton.setOnAction(event -> new TreeViewWindow(getProjects()).openTreeViewWindow(editTreeButton));

		showSpansButton.setGraphic(Icons.showSpans());
		showSpansButton.setOnAction(event -> {

			HBox group = new HBox();
			Stage stage = new Stage();
			stage.setScene(new Scene(group));

			group.getChildren().add(new TableView<>());
			stage.show();
		});

		new FilteringComboBox<>(projectSelection)//
				.withItemsUpdateOnFocus(this::getFilteredProjectsList)//
				.initialise(//
						words -> p -> matches(p, words), //
						new StringConverter<Project>() {
							@Override
							public String toString(Project proj) {
								return proj == null ? "" : fullName(proj);
							}

							@Override
							public Project fromString(String projName) {
								return projName == null || projName.isBlank() || projectTree == null ? null
										: projectSelection.getSelectionModel().getSelectedItem();
							}
						}, //
						() -> {
							buttonsStripController.startButton.requestFocus();
							buttonsStripController.startButton.fire();
						})
				.build();
	}

	private List<Project> getFilteredProjectsList() {
		if (projectTree == null)
			return Collections.emptyList();
		return projectTree.dfStream().filter(p -> p.name() != null).toList();
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

	void handleInterval(Interval t) {
		Project project = projectSelection.getValue();
		Project selectedItem = projectSelection.getSelectionModel().getSelectedItem();

		log.info("Got interval: {} {} {}", selectedItem, project, t);

		try {
			spans.add(new Span(project, t.start(), t.end()));
		} catch (IllegalArgumentException ex) {
			log.info("Ignoring invalid span: {}", ex.getMessage());
		}
	}
}
