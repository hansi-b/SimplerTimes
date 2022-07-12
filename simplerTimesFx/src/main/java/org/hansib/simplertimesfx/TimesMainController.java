package org.hansib.simplertimesfx;

import java.util.Arrays;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hansib.simplertimes.projects.Project;
import org.hansib.simplertimes.spans.Span;
import org.hansib.simplertimes.spans.SpansCollection;
import org.hansib.simplertimes.times.Interval;

import javafx.application.Platform;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.collections.transformation.FilteredList;
import javafx.fxml.FXML;
import javafx.scene.control.ComboBox;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;
import javafx.util.StringConverter;

public class TimesMainController {

	private static final Logger log = LogManager.getLogger();

	@FXML
	ComboBox<Project> projectField;

	@FXML
	ButtonsStripController buttonsStripController;

	private SpansCollection spans;
	private Project projectTree;

	@FXML
	void initialize() {
		buttonsStripController.setIntervalReceiver(this::handleInterval);
		buttonsStripController.setProjectsSupplier(() -> projectTree);

		ObservableList<Project> projectList = FXCollections.observableArrayList();

		FilteredList<Project> projectSelection = new FilteredList<>(projectList, p -> true);

		projectField.setItems(projectSelection);
		projectField.setEditable(true);

		projectField.getEditor().addEventHandler(KeyEvent.KEY_PRESSED, e -> {
			if (e.getCode() == KeyCode.ENTER) {
				buttonsStripController.startButton.requestFocus();
				buttonsStripController.startButton.fire();
			}
		});
		projectField.getEditor().textProperty().addListener((obs, oldValue, newValue) -> {
			Project selected = projectField.getSelectionModel().getSelectedItem();
			log.info("selected = {}", selected);
			Platform.runLater(() -> {
				Set<String> split = Arrays.stream(newValue.split("\\s+")).filter(s -> !s.isBlank())
						.map(String::toLowerCase).collect(Collectors.toSet());
				// If the no item in the list is selected or the selected item
				// isn't equal to the current input, we refilter the list.
				// if (selected == null || !selected.equals(cb.getEditor().getText())) {
				if (selected == null) {
					projectField.hide();
					projectSelection.setPredicate(p -> matches(p, split));
					log.info("after pred: {}", projectSelection);
					projectField.show();
				}
			});
		});
		projectField.focusedProperty().addListener((observable, oldValue, newValue) -> {
			if (Boolean.TRUE.equals(newValue)) {
				projectList.setAll(projectTree.dfStream().filter(p -> p.name() != null).toList());
			}
		});
		projectField.setConverter(new StringConverter<Project>() {
			@Override
			public String toString(Project proj) {
				return proj == null ? "" : fullName(proj);
			}

			@Override
			public Project fromString(String projName) {
				return projName == null || projName.isBlank() || projectTree == null ? null
						: projectField.getSelectionModel().getSelectedItem();
			}
		});
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
		Project val = projectField.getValue();
		Project selectedItem = projectField.getSelectionModel().getSelectedItem();

		log.info("Got interval: {} {} {}", selectedItem, val, t);

		try {
			spans.add(new Span(val, t.start(), t.end()));
		} catch (IllegalArgumentException ex) {
			log.info("Ignoring invalid span: {}", ex.getMessage());
		}
	}
}
