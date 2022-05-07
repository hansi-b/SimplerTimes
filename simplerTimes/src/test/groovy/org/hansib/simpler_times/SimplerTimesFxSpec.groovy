package org.hansib.simpler_times;

import org.hansib.simpler_times.SimplerTimesFx

import spock.lang.Specification

public class SimplerTimesFxSpec extends Specification {

	def "can get message"() {
		expect:
		new SimplerTimesFx() != null
	}
}
