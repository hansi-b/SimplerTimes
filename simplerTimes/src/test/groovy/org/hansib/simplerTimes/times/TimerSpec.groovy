package org.hansib.simplerTimes.times;

import spock.lang.Specification

public class TimerSpec extends Specification {

	def TimesRepo repo = Spy()

	def "when stopping, span is added to repo"() {

		given:
		def timer = new Timer(repo)
		timer.start()
		System.sleep(10)

		when:
		timer.stop()
		then:
		1 * repo.addSpan(*_) >> { args ->
			def started = args[0]
			def stopped = args[1]
			assert started < stopped
		}
	}

	def "cannot start when running"() {

		given:
		def timer = new Timer(repo)
		timer.start()

		when:
		timer.start()
		then:
		thrown IllegalStateException
	}

	def "cannot stop new timer"() {

		given:
		def timer = new Timer(repo)

		when:
		timer.stop()
		then:
		thrown IllegalStateException
	}
}
