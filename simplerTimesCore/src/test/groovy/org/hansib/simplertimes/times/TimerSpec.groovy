package org.hansib.simplertimes.times;

import java.time.Duration

import org.hansib.simplertimes.times.Timer

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
