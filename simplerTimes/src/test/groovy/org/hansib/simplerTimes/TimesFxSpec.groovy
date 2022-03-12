package org.hansib.simplerTimes;

import org.hansib.simplerTimes.TimesFx

import spock.lang.Specification

public class TimesFxSpec extends Specification {

	def "can get message"() {
		expect:
		new TimesFx().getMessage() == "yes"
	}
}
