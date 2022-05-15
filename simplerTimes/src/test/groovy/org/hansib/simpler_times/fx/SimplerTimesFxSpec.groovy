package org.hansib.simpler_times.fx;

import org.hansib.simpler_times.fx.SimplerTimesFx

import spock.lang.Specification

public class SimplerTimesFxSpec extends Specification {

	def "can get message"() {
		expect:
		new SimplerTimesFx() != null
	}
}
