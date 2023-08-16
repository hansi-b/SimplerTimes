package org.hansib.simplertimes.fx

import java.time.Duration
import java.time.ZonedDateTime
import java.util.function.Consumer

import org.controlsfx.control.SearchableComboBox
import org.hansib.simplertimes.projects.Project

import javafx.scene.Scene
import javafx.scene.control.Button
import javafx.scene.input.KeyCode
import javafx.scene.layout.VBox

public class SpanRecorderSpec extends AbstractAppSpec {

	FxProject root = FxProject.root(Project.root())

	FxProject alpha = root.addChild('Alpha')
	FxProject beta = root.addChild('Beta')

	SearchableComboBox<FxProject> projectSelection

	Button startButton
	Button stopButton

	List ticks = []
	List spans = []
	Consumer<Duration> tickReceiver = t -> ticks << t
	ObservableData observableData =  Mock()

	SpanRecorder recorder

	@Override
	protected Scene createScene() {

		projectSelection = new SearchableComboBox()
		projectSelection.getItems().setAll([alpha, beta])

		startButton = new Button("start")
		stopButton = new Button("stop")

		recorder = new SpanRecorder(projectSelection, startButton, stopButton, tickReceiver, () -> observableData)

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
		isEnabled projectSelection
		isEnabled startButton
		startButton.isFocused()
		isDisabled stopButton
	}

	def 'having selected project, enter starts recording'() {

		when:
		selectProjectA()
		type(KeyCode.ENTER)

		then:
		isDisabled projectSelection
		isDisabled startButton
		isEnabled stopButton
		stopButton.isFocused()

		!ticks.isEmpty()
		0 * observableData.addSpan(_, _, _)
	}

	def 'having started recording, enter registers span'() {

		given:
		def startTime = null
		def endTime = null
		def actualProject = null
		def actualStart = null
		def actualEnd = null

		when:
		selectProjectA()
		startTime = ZonedDateTime.now().minusSeconds(1)
		// start
		type(KeyCode.ENTER)
		// ... recording ...
		Thread.sleep(1000)
		// stop
		type(KeyCode.ENTER)
		endTime = ZonedDateTime.now().plusSeconds(1)

		then:
		1 * observableData.addSpan(alpha, _, _ ) >> { actualStart = it[1]; actualEnd = it[2] }
		actualStart >= startTime
		actualEnd <= endTime

		isEnabled projectSelection
		isEnabled startButton
		isDisabled stopButton
		projectSelection.isFocused()
	}

	def selectProjectA() {
		clickOn(projectSelection)
		write('a')
		type(KeyCode.ENTER)
	}

	def isDisabled(node) {
		node.isDisabled()
	}

	def isEnabled(node) {
		!node.isDisabled()
	}
}
