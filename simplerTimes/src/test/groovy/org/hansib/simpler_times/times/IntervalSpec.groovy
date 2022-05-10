package org.hansib.simpler_times.times;

import java.time.LocalDateTime

import spock.lang.Specification

public class IntervalSpec extends Specification {

	def "illegal arg for end before start"() {
		given:
		def start = LocalDateTime.now()

		when:
		new Interval(start, start.minusSeconds(10))

		then:
		thrown IllegalArgumentException
	}

	def "ensure equals"() {
		given:
		def s1 = LocalDateTime.now()
		def s2 = s1.plusMinutes(2)

		expect:
		new Interval(s1, s2).equals(new Interval(s1, s2))
		!new Interval(s1, s2).equals(new Interval(s1, s2.plusMinutes(1)))
	}
}
