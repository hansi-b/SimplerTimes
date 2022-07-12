package org.hansib.simplertimes.times;

import java.time.ZonedDateTime

import spock.lang.Specification

public class IntervalSpec extends Specification {

	def "illegal arg for end before start"() {
		given:
		def start = ZonedDateTime.now()

		when:
		new Interval(start, start.minusSeconds(10))

		then:
		thrown IllegalArgumentException
	}

	def "ensure equals"() {
		given:
		def s1 = ZonedDateTime.now()
		def s2 = s1.plusMinutes(2)

		expect:
		new Interval(s1, s2).equals(new Interval(s1, s2))
		!new Interval(s1, s2).equals(new Interval(s1, s2.plusMinutes(1)))
	}
}
