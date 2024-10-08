package org.hansib.simplertimes.times

import static org.awaitility.Awaitility.await

import java.time.Duration

import spock.lang.Specification

public class IntervalTickerSpec extends Specification {

	def manualClock = new DateTimeSource.ManualDateTime(2000)

	def 'tick receiver receives tick and last update is set'() {

		given:
		def interval = null
		def timer = new IntervalTicker(i -> {
			interval = i
		}, manualClock)

		when:
		def start = manualClock.now()
		timer.start()
		def end = manualClock.turnForward(Duration.ofSeconds(10))

		then:
		await().timeout(Duration.ofMillis(200)).until(() -> interval != null)
		interval.start == start
		interval.end == end

		timer.lastUpdate() == interval
	}

	def 'when stopping, span is returned, and last update remains zero interval'() {

		given:
		def timer = new IntervalTicker(i->{}, manualClock)

		when:
		def start = manualClock.now()
		timer.start()
		def end = manualClock.turnForward(Duration.ofMinutes(4))
		def interval = timer.stopAndGet()

		then:
		interval.start == start
		interval.end == end

		timer.lastUpdate() == new Interval(start, start)
	}

	def 'starting a started ticker throws exception'() {

		given:
		def timer = new IntervalTicker(i->{})

		when:
		timer.start()
		timer.start()

		then:
		IllegalStateException ex = thrown()
	}

	def 'can determine if ticker is started'() {

		when:
		def timer = new IntervalTicker(i->{})

		then:
		!timer.isStarted()

		when:
		timer.start()

		then:
		timer.isStarted()

		when:
		timer.stopAndGet()

		then:
		!timer.isStarted()
	}
}
