package org.hansib.simplertimesfx;

import org.hansib.simplertimesfx.SimplerTimesFx

import spock.lang.Specification

public class SimplerTimesFxSpec extends Specification {

	def "can get message"() {
		expect:
		new SimplerTimesFx() != null
	}
}
