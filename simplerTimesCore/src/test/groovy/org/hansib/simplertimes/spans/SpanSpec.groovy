package org.hansib.simplertimes.spans;

import java.time.ZoneId
import java.time.ZonedDateTime

import org.hansib.simplertimes.projects.Project

import spock.lang.Specification

public class SpanSpec extends Specification {

	Project p = Mock()

	def 'constructor rounds arguments'() {
		when:
		def s = new Span(p, ZonedDateTime.of(1,2,3,4,5,6,499999777, ZoneId.systemDefault()), ZonedDateTime.of(1,2,3,4,5,7,500000008, ZoneId.systemDefault()))
		then:
		s.start().getNano() == 0
		s.start().getSecond() == 6
		s.end().getNano() == 0
		s.end().getSecond() == 8
	}

	def 'exception on rounded constructor arguments'() {
		when:
		def s = new Span(p, ZonedDateTime.of(1,2,3,4,5,6,777, ZoneId.systemDefault()), ZonedDateTime.of(1,2,3,4,5,6,8888, ZoneId.systemDefault()))
		then:
		thrown IllegalArgumentException
	}
}
