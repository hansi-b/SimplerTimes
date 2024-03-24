package org.hansib.simplertimes.fx

import org.apache.logging.log4j.LogManager
import org.apache.logging.log4j.Logger
import org.testfx.api.FxToolkit
import org.testfx.framework.spock.ApplicationSpec
import org.testfx.util.WaitForAsyncUtils

import javafx.scene.Scene
import javafx.stage.Stage

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

	boolean isHeadless() {
		Boolean.valueOf(System.properties['testfx.headless'])
	}
}
