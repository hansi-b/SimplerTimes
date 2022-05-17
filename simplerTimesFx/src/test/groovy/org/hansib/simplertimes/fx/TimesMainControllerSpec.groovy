package org.hansib.simplertimes.fx

import java.time.LocalDateTime

import org.hansib.simplertimes.fx.TimesMainController
import org.hansib.simplertimes.spans.SpansCollection
import org.hansib.simplertimes.times.Interval
import org.hansib.simplertimes.utils.ResourceLoader
import org.testfx.framework.spock.ApplicationSpec

import javafx.fxml.FXMLLoader
import javafx.scene.Parent
import javafx.scene.Scene
import javafx.stage.Stage

public class TimesMainControllerSpec extends ApplicationSpec {

	SpansCollection spans = Mock()

	TimesMainController controller

	@Override
	public void start(Stage stage) throws Exception {

		final FXMLLoader fxmlLoader = ResourceLoader.get().getFxmlLoader("timesMain.fxml")
		Parent root = fxmlLoader.load()
		controller = (TimesMainController) fxmlLoader.getController()
		controller.setSpans(spans)

		Scene scene = new Scene(root)
		stage.setScene(scene)
		stage.show()
	}

	def 'valid span is added'() {

		given:
		def ldt = LocalDateTime.now()
		controller.projectField.setText('wired')

		when:
		controller.handleInterval(new Interval(ldt.minusSeconds(2), ldt))

		then:
		1 * spans.add(_)
	}

	def 'invalid span is not added'() {

		given:
		def ldt = LocalDateTime.now()
		controller.projectField.setText('wired')

		when:
		controller.handleInterval(new Interval(ldt.minusNanos(1000), ldt))

		then:
		0 * spans.add(_)
	}
}