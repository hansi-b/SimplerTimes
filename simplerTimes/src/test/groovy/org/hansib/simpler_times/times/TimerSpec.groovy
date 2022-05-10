package org.hansib.simpler_times.times;

import java.time.Duration

import spock.lang.Specification

public class TimerSpec extends Specification {

	def "when stopping, span is returned"() {

		given:
		def timer = Timer.start()
		System.sleep(10)

		when:
		def i = timer.interval()

		then:
		i.end() > i.start()
		timer.duration() >= Duration.between(i.start(), i.end())
	}
}
