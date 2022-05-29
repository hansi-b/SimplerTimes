package org.hansib.simplertimes.spans

import java.time.OffsetDateTime

import spock.lang.Specification

public class SpansYamlConverterSpec extends Specification {

	def now = OffsetDateTime.now()
	def s1 = new Span("abc", now.minusMinutes(1), now)
	def s2 = new Span("xyz", now.minusHours(2), now.minusHours(1))
	def s3 = new Span("uvw", now.minusHours(2), now.minusHours(1))

	def 'can de/serialize'() {

		when:
		def sc = SpansCollection.with(s1, s2)

		then:
		SpansYamlConverter.fromYaml(SpansYamlConverter.toYaml(sc)).equals(sc)
	}
}
