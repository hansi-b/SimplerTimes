package org.hansib.simplertimesfx;

import java.time.Duration;
import java.util.function.Consumer;

import org.hansib.simplertimes.Project;
import org.hansib.simplertimes.times.Interval;
import org.hansib.simplertimes.times.TimerDisplay;
import org.hansib.simplertimes.tree.TreeNode;
import org.hansib.simplertimesfx.tree.TreeViewWindow;

import javafx.application.Platform;
import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.Label;

public class ButtonsStripController {

	@FXML
	Button startButton;

	@FXML
	Button stopButton;

	@FXML
	Label elapsedTime;

	@FXML
	Button editTreeButton;

	private TimerDisplay timerDisplay;

	private Consumer<Interval> intervalReceiver;

	@FXML
	void initialize() {
		timerDisplay = new TimerDisplay(this::updateTime);

		startButton.setGraphic(Icons.start());
		stopButton.setGraphic(Icons.stop());

		stopButton.setDisable(true);

		editTreeButton.setGraphic(Icons.editTree());
		editTreeButton.setOnAction(event -> TreeViewWindow.openTreeViewWindow(editTreeButton, buildTree()));

		startButton.setOnAction(a -> startTiming());
		stopButton.setOnAction(a -> stopTiming());
	}

	private static TreeNode<Project> buildTree() {
		TreeNode<Project> root = TreeNode.root();
		TreeNode<Project> book = root.add(new Project("Book"));
		book.add(new Project("Chapter 1"));
		book.add(new Project("Chapter 2"));
		TreeNode<Project> stuff = root.add(new Project("Stuff"));
		stuff.add(new Project("clean"));
		return root;
	}

	private void updateTime(Duration duration) {
		Platform.runLater(() -> elapsedTime.setText(
				String.format("%d:%02d:%02d", duration.toHours(), duration.toMinutesPart(), duration.toSecondsPart())));
	}

	void setIntervalReceiver(Consumer<Interval> intervalReceiver) {
		this.intervalReceiver = intervalReceiver;
	}

	private void startTiming() {
		startButton.setDisable(true);
		stopButton.setDisable(false);
		timerDisplay.start();
	}

	private void stopTiming() {
		stopButton.setDisable(true);
		startButton.setDisable(false);

		intervalReceiver.accept(timerDisplay.stopAndGet());
	}
}
