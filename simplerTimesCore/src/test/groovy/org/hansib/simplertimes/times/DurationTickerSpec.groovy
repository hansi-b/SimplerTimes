package org.hansib.simplertimes.times;

import java.time.Duration

import spock.lang.Specification

public class DurationTickerSpec extends Specification {

	def "when stopping, span is returned"() {

		given:
		def duration = null
		def timer = new DurationTicker(d->{
			duration = d;
		})

		when:
		timer.start()
		System.sleep(10)
		def i = timer.stopAndGet()

		then:
		i.end() > i.start()
		duration <= Duration.between(i.start(), i.end())
	}
}
