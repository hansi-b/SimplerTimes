package org.hansib.simpler_times

import java.time.LocalDateTime

import spock.lang.Specification

public class SpansCollectionSpec extends Specification {

	def 'can de/serialize'() {

		when:
		def now = LocalDateTime.now()
		def s1 = new Span("abc", now.minusMinutes(1), now)
		def s2 = new Span("abc", now.minusHours(2), now.minusHours(1))

		def sc = new SpansCollection()
		sc.add(s1)
		sc.add(s2)

		then:
		SpansCollection.fromYaml(sc.toYaml()).equals(sc)
	}
}
