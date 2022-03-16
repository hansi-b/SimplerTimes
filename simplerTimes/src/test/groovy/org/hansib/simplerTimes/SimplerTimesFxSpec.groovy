package org.hansib.simplerTimes;

import org.hansib.simplerTimes.SimplerTimesFx

import spock.lang.Specification

public class SimplerTimesFxSpec extends Specification {

	def "can get message"() {
		expect:
		new SimplerTimesFx().getMessage() == "yes"
	}
}
