package org.hansib.simplertimesfx

import java.time.Duration
import java.util.function.Consumer

import org.hansib.simplertimes.projects.Project
import org.hansib.simplertimes.spans.Span

import javafx.scene.Scene
import javafx.scene.control.Button
import javafx.scene.control.ComboBox
import javafx.scene.layout.VBox
import spock.lang.Shared

public class SpanRecorderSpec extends AbstractAppSpec {

	@Shared
	Project alpha
	@Shared
	Project beta

	ComboBox<Project> projectSelection

	Button startButton
	Button stopButton

	Consumer<Duration> tickReceiver
	Consumer<Span> spanReceiver

	SpanRecorder recorder

	void setupSpec() {
		Project root = Project.root()
		alpha = root.add('Alpha')
		beta = root.add('Beta')
	}

	@Override
	protected Scene createScene() {

		projectSelection = new ComboBox()
		startButton = new Button("start")
		stopButton = new Button("stop")

		return new Scene(new VBox(projectSelection, startButton, stopButton))
	}

	def 'initially, selection is empty and buttons disabled'() {

		given:
		projectSelection.getItems().setAll([alpha, beta])

		when:
		tickReceiver = t -> print t
		spanReceiver = s -> print s
		waitForAsyncFx(() -> {
			recorder = new SpanRecorder(projectSelection, startButton, stopButton, tickReceiver, spanReceiver)
		})

		then:
		projectSelection.getSelectionModel().getSelectedIndex() == -1
		startButton.isDisabled()
		stopButton.isDisabled()
	}
}
