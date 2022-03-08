package javafx_app.app;

import spock.lang.Specification

public class MessageUtilsSpec extends Specification {

	def "get hello world"() {
		expect:
		MessageUtils.getMessage() == "Hello    World!"
	}
}
