package org.hansib.simplertimesfx

import java.time.Duration
import java.util.function.Consumer

import org.controlsfx.control.SearchableComboBox
import org.hansib.simplertimes.projects.Project
import org.hansib.simplertimes.spans.Span

import javafx.scene.Scene
import javafx.scene.control.Button
import javafx.scene.input.KeyCode
import javafx.scene.layout.VBox

public class SpanRecorderSpec extends AbstractAppSpec {

	Project root = Project.root()
	Project alpha = root.add('Alpha')
	Project beta = root.add('Beta')

	SearchableComboBox<Project> projectSelection

	Button startButton
	Button stopButton

	List ticks = []
	List spans = []
	Consumer<Duration> tickReceiver = t -> ticks << t
	Consumer<Span> spanReceiver = s -> spans << s

	SpanRecorder recorder

	@Override
	protected Scene createScene() {

		projectSelection = new SearchableComboBox()
		projectSelection.getItems().setAll([alpha, beta])

		startButton = new Button("start")
		stopButton = new Button("stop")

		recorder = new SpanRecorder(projectSelection, startButton, stopButton, tickReceiver, spanReceiver)

		return new Scene(new VBox(projectSelection, startButton, stopButton))
	}

	def 'initially, selection is empty and buttons disabled'() {

		expect:
		projectSelection.getSelectionModel().getSelectedIndex() == -1
		startButton.isDisabled()
		stopButton.isDisabled()
	}

	def 'can select project'() {

		when:
		clickOn(projectSelection)
		write('a')
		type(KeyCode.ENTER)

		then:
		projectSelection.getSelectionModel().getSelectedIndex() == 0
		!startButton.isDisabled()
		stopButton.isDisabled()
	}
}
