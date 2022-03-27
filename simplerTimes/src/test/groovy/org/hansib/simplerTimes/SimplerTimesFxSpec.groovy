package org.hansib.simplerTimes;

import spock.lang.Specification

public class SimplerTimesFxSpec extends Specification {

	def "can get message"() {
		expect:
		new SimplerTimesFx() != null
	}
}
