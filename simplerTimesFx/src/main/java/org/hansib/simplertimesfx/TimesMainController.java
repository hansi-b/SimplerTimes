package org.hansib.simplertimesfx;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hansib.simplertimes.projects.Project;
import org.hansib.simplertimes.spans.Span;
import org.hansib.simplertimes.spans.SpansCollection;
import org.hansib.simplertimes.times.Interval;

import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
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
	private Project projects;

	@FXML
	void initialize() {
		buttonsStripController.setIntervalReceiver(this::handleInterval);
		buttonsStripController.setProjectsSupplier(() -> projects);

		projectField.setEditable(true);
		projectField.getEditor().addEventHandler(KeyEvent.KEY_PRESSED, e -> {
			if (e.getCode() == KeyCode.ENTER) {
				buttonsStripController.startButton.requestFocus();
				buttonsStripController.startButton.fire();
			}
		});
		projectField.focusedProperty().addListener(new ChangeListener<Boolean>() {
			@Override
			public void changed(ObservableValue<? extends Boolean> observable, Boolean oldValue, Boolean newValue) {
				if (Boolean.TRUE.equals(newValue)) {
					projectField.setItems(getFilteredProjects());
				}
			}
		});
		projectField.setConverter(new StringConverter<Project>() {
			@Override
			public String toString(Project proj) {
				return proj == null ? "" : proj.fullProjectName();
			}

			@Override
			public Project fromString(String projName) {
				return projName == null || projName.isBlank() || projects == null ? null : projects.add(projName);
			}
		});
	}

	private ObservableList<Project> getFilteredProjects() {
		return FXCollections.observableArrayList(projects.dfStream().filter(p -> p.name() != null).toList());
	}

	void setSpans(SpansCollection spans) {
		this.spans = spans;
	}

	public void setProjects(Project projects) {
		this.projects = projects;
	}

	SpansCollection getSpans() {
		return spans;
	}

	Project getProjects() {
		return projects;
	}

	void handleInterval(Interval t) {
		log.info("Got interval: {} {}", projectField.getEditor().getText(), t);
		try {
			spans.add(new Span(projectField.getValue(), t.start(), t.end()));
		} catch (IllegalArgumentException ex) {
			log.info("Ignoring invalid span: {}", ex.getMessage());
		}
	}
}
