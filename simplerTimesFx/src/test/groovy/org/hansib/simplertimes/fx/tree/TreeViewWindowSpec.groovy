package org.hansib.simplertimes.fx.tree

import javafx.scene.Scene
import javafx.scene.control.MenuItem
import javafx.scene.input.KeyCode

import org.hansib.simplertimes.fx.AbstractAppSpec
import org.hansib.simplertimes.fx.data.FxProject
import org.hansib.simplertimes.fx.data.ObservableData
import org.hansib.simplertimes.fx.l10n.L10nSetup
import org.hansib.simplertimes.projects.Project
import org.testfx.util.WaitForAsyncUtils
import spock.lang.IgnoreIf

public class TreeViewWindowSpec extends AbstractAppSpec {

	Project root
	ObservableData data

	TreeViewWindow treeWindow

	def 'setupSpec'() {
		L10nSetup.activateEnglish()
	}

	@Override
	protected Scene createScene() {
		root = Project.root()
		root.add('First')

		def fxRoot = FxProject.root(root)

		data = new ObservableData(fxRoot, Collections.emptyList())

		treeWindow = new TreeViewWindow(fxRoot, data::updateProjectList)
		treeWindow.setPreRemovalChecker(data.fxProjectRemovalCallback())
		
		def stage = treeWindow.initStage()
		return stage.getScene()
	}

	def 'can rename project'() {

		when:
		doubleClickOn 'First'
		write 'Modified'
		type KeyCode.ENTER

		then:
		root.children.size() == 1
		root.children[0].name == 'Modified'
	}

	/**
	 * Ignored in headless mode because on the pane of the tree view,
	 * we cannot apply the hack to show the context menu. 	*/
	@IgnoreIf({ isHeadless() })
	def 'can add second project'() {

		given:
		givenSecondProject()

		expect:
		root.children.size() == 2
		root.children[0].name == 'First'
		root.children[1].name == 'Second'
	}


	def 'can add subproject'() {

		when:
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
		assert child.children[0].name == 'Second': "Got $child with ${child.children}"
	}

	@IgnoreIf({ isHeadless() })
	def 'can delete project with warning'() {

		given:
		givenSecondProject()

		when:
		rightClick lookup('First')

		// in context menu:
		clickOn 'Delete'

		// repeat on alert dialogue:
		clickOn 'Delete'

		then:
		root.children.size() == 1
		root.children[0].name == 'Second'
	}

	def givenSecondProject() {
		def node = lookup("#${TreeViewWindow.PROJECT_PANE_FX_ID}").query()
		rightClickOn node

		if (isHeadless()) {
			WaitForAsyncUtils.asyncFx {
				// does not work in headless mode
				// unclear how to get the context menu visible on the pane
			}.get()
		}
		clickOn lookup('New Project').query()
		write 'Second'
		type KeyCode.ENTER
	}

	def 'cannot delete only project'() {

		when:
		rightClick lookup('First')

		then:
		MenuItem item = lookup('Delete').query().getLabelFor().item
		item.isDisable()
	}
}
