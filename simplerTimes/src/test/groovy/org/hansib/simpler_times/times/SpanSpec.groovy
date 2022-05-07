package org.hansib.simpler_times.times;

import java.time.LocalDateTime

import spock.lang.Specification

public class SpanSpec extends Specification {

	def "zero-length span is legal"() {
		given:
		def start = LocalDateTime.now()

		when:
		def s = new Span(start, start)

		then:
		s.start() == s.end()
	}

	def "illegal arg for end before start"() {
		given:
		def start = LocalDateTime.now()

		when:
		new Span(start, start.minusSeconds(10))

		then:
		thrown IllegalArgumentException
	}

	def "ensure equals"() {
		given:
		def s1 = LocalDateTime.now()
		def s2 = s1.plusMinutes(2)

		expect:
		new Span(s1, s2).equals(new Span(s1, s2))
		!new Span(s1, s2).equals(new Span(s1, s2.plusMinutes(1)))
	}
}
