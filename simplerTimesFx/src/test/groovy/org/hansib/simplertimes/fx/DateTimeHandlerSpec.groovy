package org.hansib.simplertimes.fx;

import java.time.LocalDateTime
import java.time.OffsetDateTime

import spock.lang.Specification

public class DateTimeHandlerSpec extends Specification {

	DateTimeHandler dtHandler = new DateTimeHandler()

	def 'can parse and fail gracefully'() {

		expect:
		dtHandler.parseToOffsetDateTime('44-55-1234 12:23:44') == null
		dtHandler.parseToOffsetDateTime('07-03-1999 09:10:11') == OffsetDateTime.of(LocalDateTime.of(1999, 3, 7, 9, 10, 11),
				OffsetDateTime.now().getOffset())
	}
}
