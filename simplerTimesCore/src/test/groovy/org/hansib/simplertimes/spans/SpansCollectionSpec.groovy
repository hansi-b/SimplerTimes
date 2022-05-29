package org.hansib.simplertimes.spans

import java.time.OffsetDateTime

import spock.lang.Specification

public class SpansCollectionSpec extends Specification {

	def now = OffsetDateTime.now()
	def s1 = new Span("abc", now.minusMinutes(1), now)
	def s2 = new Span("xyz", now.minusHours(2), now.minusHours(1))
	def s3 = new Span("uvw", now.minusHours(2), now.minusHours(1))

	def 'equals'() {

		expect:
		SpansCollection.with(s1, s2).equals(SpansCollection.with(s1, s2))
		!SpansCollection.with(s1, s2).equals(SpansCollection.with(s1, s3))
	}
}
