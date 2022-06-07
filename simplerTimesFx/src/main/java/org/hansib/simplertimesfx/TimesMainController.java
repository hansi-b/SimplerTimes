package org.hansib.simplertimesfx;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hansib.simplertimes.Project;
import org.hansib.simplertimes.spans.Span;
import org.hansib.simplertimes.spans.SpansCollection;
import org.hansib.simplertimes.times.Interval;
import org.hansib.simplertimes.tree.TreeNode;

import javafx.fxml.FXML;
import javafx.scene.control.TextField;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;

public class TimesMainController {

	private static final Logger log = LogManager.getLogger();

	@FXML
	TextField projectField;

	@FXML
	ButtonsStripController buttonsStripController;

	private SpansCollection spans;
	private TreeNode<Project> projects;

	@FXML
	void initialize() {
		buttonsStripController.setIntervalReceiver(this::handleInterval);
		buttonsStripController.setProjectsSupplier(() -> projects);

		projectField.addEventHandler(KeyEvent.KEY_PRESSED, e -> {
			if (e.getCode() == KeyCode.ENTER) {
				buttonsStripController.startButton.requestFocus();
				buttonsStripController.startButton.fire();
			}
		});
	}

	void setSpans(SpansCollection spans) {
		this.spans = spans;
	}

	public void setProjects(TreeNode<Project> projects) {
		this.projects = projects;
	}

	SpansCollection getSpans() {
		return spans;
	}

	TreeNode<Project> getProjects() {
		return projects;
	}

	void handleInterval(Interval t) {
		log.info("Got interval: {} {}", projectField.getText(), t);
		try {
			spans.add(new Span(projectField.getText(), t.start(), t.end()));
		} catch (IllegalArgumentException ex) {
			log.info("Ignoring invalid span: {}", ex.getMessage());
		}
	}
}
