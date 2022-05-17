package org.hansib.simplertimes.spans;

import java.time.LocalDateTime

import org.hansib.simplertimes.spans.Span

import spock.lang.Specification

public class SpanSpec extends Specification {

	def 'constructor rounds arguments'() {
		when:
		def s = new Span("a", LocalDateTime.of(1,2,3,4,5,6,444999777), LocalDateTime.of(1,2,3,4,5,6,500000008))
		then:
		s.start().getNano() == 0
		s.start().getSecond() == 6
		s.end().getNano() == 0
		s.end().getSecond() == 7
	}

	def 'exception on rounded constructor arguments'() {
		when:
		def s = new Span("a", LocalDateTime.of(1,2,3,4,5,6,777), LocalDateTime.of(1,2,3,4,5,6,8888))
		then:
		thrown IllegalArgumentException
	}
}
