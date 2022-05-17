package org.hansib.simplertimes.fx;

import org.hansib.simplertimes.fx.SimplerTimesFx

import spock.lang.Specification

public class SimplerTimesFxSpec extends Specification {

	def "can get message"() {
		expect:
		new SimplerTimesFx() != null
	}
}
