package org.hansib.simplertimes.times

import java.time.Duration

import spock.lang.Specification

public class DurationTickerSpec extends Specification {

	def manualClock = new DateTimeSource.ManualDateTime()

	def 'tick receiver receives tick'() {

		given:
		def duration = null
		def timer = new DurationTicker(d -> {
			duration = d
		}, manualClock)

		when:
		timer.start()
		manualClock.turnForward(Duration.ofSeconds(10))
		System.sleep(60)
		timer.stopAndGet()

		then:
		duration == Duration.ofSeconds(10)
	}

	def 'when stopping, span is returned'() {

		given:
		def timer = new DurationTicker(d->{}, manualClock)

		when:
		def start = manualClock.now()
		timer.start()
		def end = manualClock.turnForward(Duration.ofMinutes(4))
		def interval = timer.stopAndGet()

		then:
		interval.start == start
		interval.end == end
	}

	def 'starting a started ticker throws exception'() {

		given:
		def timer = new DurationTicker(d->{})

		when:
		timer.start()
		timer.start()

		then:
		IllegalStateException ex = thrown()
	}
}
