package org.hansib.simplertimes.fx

import org.apache.logging.log4j.LogManager
import org.apache.logging.log4j.Logger
import org.testfx.api.FxToolkit
import org.testfx.framework.spock.ApplicationSpec
import org.testfx.util.WaitForAsyncUtils

import javafx.scene.Scene
import javafx.stage.Stage

/**
 * This is a copy of the AppSpecWithScene in SundriesFx. Will want to unify that at some point.
 */
abstract public class AbstractAppSpec extends ApplicationSpec {
	static final Logger log = LogManager.getLogger(AbstractAppSpec)

	protected Stage stage

	@Override
	public void start(Stage stage) throws Exception {
		this.stage = FxToolkit.registerPrimaryStage()
		FxToolkit.setupScene(() -> createScene())
		FxToolkit.showStage()
	}

	protected abstract Scene createScene();

	public static void waitForAsyncFx(Runnable runnable) {
		WaitForAsyncUtils.waitForAsyncFx(1_000, runnable)
	}

	@Override
	void stop() throws Exception {
		FxToolkit.cleanupStages()
	}

	static boolean isHeadless() { Boolean.getBoolean('testfx.headless') }

	/**
	 * Right-click on the argument element; adds a hack from
	 * https://github.com/TestFX/Monocle/issues/12#issuecomment-341795874 for headless mode.
	 *
	 * NB: Does not work in headless mode for elements that can be right-clicked but
	 * don't have a contextMenu getter (e.g., a pane).
	 *
	 * @param lookup the element to query
	 * @return the element which was right-clicked
	 */
	def rightClick(lookup) {
		def node = lookup.query()
		rightClickOn node
		if (isHeadless()) {
			WaitForAsyncUtils.asyncFx {
				node.contextMenu.show(stage.scene.window)
			}.get()
		}
		return node
	}
}
