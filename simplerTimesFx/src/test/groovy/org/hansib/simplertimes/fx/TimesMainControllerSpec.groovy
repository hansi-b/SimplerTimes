package org.hansib.simplertimes.fx

import org.hansib.simplertimes.projects.Project
import org.hansib.simplertimes.spans.SpansCollection
import org.hansib.sundries.fx.FxResourceLoader
import org.testfx.framework.spock.ApplicationSpec

import javafx.stage.Stage
import spock.lang.Ignore

@Ignore
public class TimesMainControllerSpec extends ApplicationSpec {

	SpansCollection spans = Mock()
	Project rootProject = Project.root()

	TimesMainController controller

	@Override
	public void start(Stage stage) throws Exception {

		controller = new FxResourceLoader().loadFxmlToStage("timesMain.fxml", stage)

		controller.setProjects(rootProject)
		controller.setSpans(spans)

		stage.show()
	}
}
