package org.hansib.simplertimes.times

import static org.awaitility.Awaitility.await

import java.time.Duration

import spock.lang.Specification

public class DurationTickerSpec extends Specification {

	def manualClock = new DateTimeSource.ManualDateTime(2000)

	def 'tick receiver receives tick'() {

		given:
		def interval = null
		def timer = new DurationTicker(i -> {
			interval = i
		}, manualClock)

		when:
		def start = manualClock.now()
		timer.start()
		def end = manualClock.turnForward(Duration.ofSeconds(10))

		then:
		await().pollDelay(Duration.ofMillis(20)).timeout(Duration.ofMillis(200)).until(() -> interval != null)
		interval.start == start
		interval.end == end
	}

	def 'when stopping, span is returned'() {

		given:
		def timer = new DurationTicker(i->{}, manualClock)

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
		def timer = new DurationTicker(i->{})

		when:
		timer.start()
		timer.start()

		then:
		IllegalStateException ex = thrown()
	}
}
