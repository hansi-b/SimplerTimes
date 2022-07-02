package org.hansib.simplertimes.yaml

import java.time.OffsetDateTime

import org.hansib.simplertimes.projects.Project
import org.hansib.simplertimes.spans.Span
import org.hansib.simplertimes.spans.SpansCollection

import spock.lang.Specification

public class SpansYamlConverterSpec extends Specification {

	Project root = Project.root()
	Project a = root.add('a')
	Project x = a.add('x')
	Project b = root.add('b')

	def now = OffsetDateTime.now()
	def s1 = new Span(x, now.minusMinutes(1), now)
	def s2 = new Span(y, now.minusHours(2), now.minusHours(1))
	def s3 = new Span(z, now.minusHours(2), now.minusHours(1))

	def 'can de/serialize'() {

		when:
		def sc = SpansCollection.with(s1, s2)

		then:
		SpansYamlConverter.fromYaml(SpansYamlConverter.toYaml(sc)).equals(sc)
	}


	def 'can serialize'() {

		when:
		def sc = SpansCollection.with(s1, s2)

		then:
		SpansYamlConverter.fromYaml(SpansYamlConverter.toYaml(sc)).equals(sc)
	}
}
