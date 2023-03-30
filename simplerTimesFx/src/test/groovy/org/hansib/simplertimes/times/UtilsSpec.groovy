package org.hansib.simplertimes.times;

import java.time.LocalDate

import spock.lang.Specification

public class UtilsSpec extends Specification {

	def "starting Monday gives same week"() {
		when:
		def days = Utils.daysOfWeek(LocalDate.of(2023, 3, 27)) // Monday
		then:
		days.size() == 7
		days.first() == LocalDate.of(2023, 3, 27)
		days.last() == LocalDate.of(2023, 4, 2)
	}

	def "can get days of week starting Wednesday"() {
		when:
		def days = Utils.daysOfWeek(LocalDate.of(2023, 3, 29)) // Wednesday
		then:
		days.size() == 7
		days.first() == LocalDate.of(2023, 3, 27)
		days.last() == LocalDate.of(2023, 4, 2)
	}
}
