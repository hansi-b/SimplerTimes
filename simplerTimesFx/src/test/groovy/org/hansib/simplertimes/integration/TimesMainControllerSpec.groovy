package org.hansib.simplertimes.integration

import org.hansib.simplertimes.DataStore
import org.hansib.simplertimes.fx.AbstractAppSpec
import org.hansib.simplertimes.fx.TimesMainController
import org.hansib.simplertimes.fx.data.ObservableData
import org.hansib.simplertimes.fx.l10n.L10nSetup
import org.hansib.simplertimes.projects.Project
import org.hansib.simplertimes.spans.SpansCollection
import org.hansib.sundries.fx.FxResourceLoader
import org.testfx.util.WaitForAsyncUtils

import javafx.scene.Scene
import javafx.scene.input.KeyCode
import javafx.scene.input.MouseButton

public class TimesMainControllerSpec extends AbstractAppSpec {

	SpansCollection spans
	Project projects

	DataStore dataStore = Mock()

	// object under test
	TimesMainController controller

	@Override
	protected Scene createScene() {
		controller = new FxResourceLoader().loadFxmlToStage("timesMain.fxml", stage)
		return stage.getScene()
	}

	def 'setupSpec'() {
		L10nSetup.activateEnglish()
	}

	def 'setup'() {

		projects = Project.root()
		projects.add("New Project")

		spans = new SpansCollection()

		dataStore.loadProjectTree() >> projects
		dataStore.loadSpans(_) >> spans

		controller.setData(ObservableData.load(dataStore))
	}

	def 'can edit project tree'() {

		when:
		clickOn '#editTreeButton'
		doubleClickOn 'New Project'
		write 'First'
		type KeyCode.ENTER

		def node = lookup('First').query()
		clickOn node, MouseButton.SECONDARY
		if (isHeadless()) {
			/*
			 * hack from
			 * https://github.com/TestFX/Monocle/issues/12#issuecomment-341795874
			 */
			WaitForAsyncUtils.asyncFx { node.contextMenu.show(stage.scene.window) }.get()
		}
		clickOn 'New Subproject'

		/* auto-selection of new node does not work in headless mode */
		if (isHeadless()) {
			clickOn lookup('New Project').query()
			push(KeyCode.CONTROL, KeyCode.A)
		}

		write 'Second'
		type KeyCode.ENTER

		then:
		projects.children.size() == 1
		def child = projects.children[0]
		child.name == 'First'
		child.children.size() == 1
		assert child.children[0].name == 'Second' : "Got $child with ${child.children}"
	}
}
