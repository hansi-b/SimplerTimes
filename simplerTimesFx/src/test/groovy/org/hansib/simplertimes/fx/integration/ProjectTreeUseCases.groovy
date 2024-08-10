package org.hansib.simplertimes.fx.integration

import org.hansib.simplertimes.DataStore
import org.hansib.simplertimes.fx.AbstractAppSpec
import org.hansib.simplertimes.fx.TimesMainController
import org.hansib.simplertimes.fx.data.ObservableData
import org.hansib.simplertimes.fx.l10n.L10nSetup
import org.hansib.simplertimes.fx.tree.TreeViewWindow
import org.hansib.simplertimes.projects.Project
import org.hansib.simplertimes.spans.SpansCollection
import org.hansib.sundries.fx.FxResourceLoader
import org.testfx.util.WaitForAsyncUtils

import javafx.scene.Scene
import javafx.scene.control.MenuItem
import javafx.scene.input.KeyCode
import javafx.scene.input.MouseButton
import spock.lang.IgnoreIf

public class ProjectTreeUseCases extends AbstractAppSpec {

	SpansCollection spans
	Project root

	DataStore dataStore = Mock()

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

		root = Project.root()
		root.add('First')

		spans = new SpansCollection()

		dataStore.loadProjectTree() >> root
		dataStore.loadSpans(_) >> spans

		controller.setData(ObservableData.load(dataStore))
	}

	def 'can rename project'() {

		when:
		clickOn '#editTreeButton'
		doubleClickOn 'First'
		write 'Modified'
		type KeyCode.ENTER

		then:
		root.children.size() == 1
		root.children[0].name == 'Modified'
	}

	/*
	 * Ignored in headless mode because on the pane of the tree view,
	 * we cannot apply the hack to show the context menu. 
	 */
	@IgnoreIf({ isHeadless() })
	def 'can add second project'() {

		when:
		clickOn '#editTreeButton'

		def node = lookup("#${TreeViewWindow.PROJECT_PANE_FX_ID}").query()
		rightClickOn node
		if (isHeadless()) {
			WaitForAsyncUtils.asyncFx {
				// unclear how to get the context menu visible on the pane
			}.get()
		}

		clickOn lookup('New Project').query()
		/* auto-selection of new node does not work in headless mode */
		if (isHeadless()) {
			clickOn MouseButton.PRIMARY
			push(KeyCode.CONTROL, KeyCode.A)
		}

		write 'Second'
		type KeyCode.ENTER

		then:
		root.children.size() == 2
		root.children[0].name == 'First'
		root.children[1].name == 'Second'
	}

	def 'can add subproject'() {

		when:
		clickOn '#editTreeButton'

		rightClick lookup('First')

		clickOn 'New Subproject'
		/* auto-selection of new node does not work in headless mode */
		if (isHeadless()) {
			clickOn lookup('New Project').query()
			push(KeyCode.CONTROL, KeyCode.A)
		}

		write 'Second'
		type KeyCode.ENTER

		then:
		root.children.size() == 1
		def child = root.children[0]
		child.children.size() == 1
		assert child.children[0].name == 'Second' : "Got $child with ${child.children}"
	}

	def 'can delete project with warning'() {

		given:
		root.add('Second Project')
		controller.setData(ObservableData.load(dataStore))

		when:
		clickOn '#editTreeButton'

		rightClick lookup('First')

		// in context menu:
		clickOn 'Delete'

		// repeat on alert dialogue:
		clickOn 'Delete'

		then:
		root.children.size() == 1
		root.children[0].name == 'Second Project'
	}

	def 'cannot delete only project'() {

		when:
		clickOn '#editTreeButton'

		rightClick lookup('First')

		then:
		MenuItem item = lookup('Delete').query().getLabelFor().item
		item.isDisable() == true
	}
}
