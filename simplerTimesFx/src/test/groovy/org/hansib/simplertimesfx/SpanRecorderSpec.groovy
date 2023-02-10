package org.hansib.simplertimesfx

import java.time.Duration
import java.time.OffsetDateTime
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
		ticks.isEmpty()
		spans.isEmpty()
	}

	def 'can select project'() {

		when:
		selectProjectA()

		then:
		projectSelection.getSelectionModel().getSelectedIndex() == 0
		startButton.isFocused()
		startButton.isDisabled() == false
		stopButton.isDisabled() == true
	}

	def 'having selected project, enter starts recording'() {

		when:
		selectProjectA()
		type(KeyCode.ENTER)

		then:
		startButton.isDisabled() == true
		stopButton.isFocused()
		stopButton.isDisabled() == false

		!ticks.isEmpty()
		spans.isEmpty()
	}

	def 'having started recording, enter registers span'() {

		when:
		selectProjectA()
		def startTime = OffsetDateTime.now().minusSeconds(1)
		// start
		type(KeyCode.ENTER)
		// ... recording ...
		Thread.sleep(1000)
		// stop
		type(KeyCode.ENTER)
		def endTime = OffsetDateTime.now().plusSeconds(1)

		then:
		spans.size() == 1
		spans[0].start >= startTime
		spans[0].end <= endTime

		startButton.isDisabled() == false
		stopButton.isDisabled() == true
		projectSelection.isFocused()
	}

	def selectProjectA() {
		clickOn(projectSelection)
		write('a')
		type(KeyCode.ENTER)
	}
}
