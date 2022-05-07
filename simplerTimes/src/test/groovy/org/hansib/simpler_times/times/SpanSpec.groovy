package org.hansib.simpler_times.times;

import java.time.LocalDateTime

import org.hansib.simpler_times.times.Span

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
}
