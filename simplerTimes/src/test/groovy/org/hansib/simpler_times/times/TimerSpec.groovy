package org.hansib.simpler_times.times;

import org.hansib.simpler_times.times.Timer

import spock.lang.Specification

public class TimerSpec extends Specification {

	def "when stopping, span is returned"() {

		given:
		def timer = new Timer()
		def started = timer.start()
		System.sleep(10)

		when:
		def span = timer.stopAndGet()

		then:
		span.start() == started
		span.end() > span.start()
	}

	def "cannot start when running"() {

		given:
		def timer = new Timer()
		timer.start()

		when:
		timer.start()
		then:
		thrown IllegalStateException
	}

	def "cannot stop new timer"() {

		given:
		def timer = new Timer()

		when:
		timer.stopAndGet()
		then:
		thrown IllegalStateException
	}
}
