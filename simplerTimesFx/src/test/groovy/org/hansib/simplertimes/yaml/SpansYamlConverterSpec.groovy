package org.hansib.simplertimes.yaml

import java.time.OffsetDateTime
import java.time.ZoneOffset

import org.hansib.simplertimes.projects.Project
import org.hansib.simplertimes.spans.Span
import org.hansib.simplertimes.spans.SpansCollection

import spock.lang.Specification

public class SpansYamlConverterSpec extends Specification {

	Project root = Project.root()
	Project a = root.add('a')
	Project y = root.add('y')
	Project b = root.add('b')
	Project x = a.add('x')

	def start = OffsetDateTime.of(2022,5,24,8,55,00,0,ZoneOffset.UTC)
	def s1 = new Span(x, start.minusMinutes(1), start)
	def s2 = new Span(y, start.minusHours(2), start.minusHours(1))
	def s3 = new Span(b, start.minusHours(1), start.minusMinutes(30))

	def 'serialization sorts'() {

		when:
		def sc = SpansCollection.with(s1, s2, s3)

		then:
		SpansYamlConverter.toYaml(sc) == """---
- projectId: ${y.id}
  start: "2022-05-24T06:55:00Z"
  end: "2022-05-24T07:55:00Z"
- projectId: ${b.id}
  start: "2022-05-24T07:55:00Z"
  end: "2022-05-24T08:25:00Z"
- projectId: ${x.id}
  start: "2022-05-24T08:54:00Z"
  end: "2022-05-24T08:55:00Z"
"""
	}

	def 'can deserialize unsorted'() {

		when:
		String yaml = """---
- projectId: ${x.id}
  start: "2022-05-24T08:54:00Z"
  end: "2022-05-24T08:55:00Z"
- projectId: ${y.id}
  start: "2022-05-24T06:55:00Z"
  end: "2022-05-24T07:55:00Z"
- projectId: ${b.id}
  start: "2022-05-24T07:55:00Z"
  end: "2022-05-24T08:25:00Z"
"""
		SpansCollection sc = SpansYamlConverter.fromYaml(root, yaml)
		List<Span> spans = sc.stream().toList();

		then:
		spans.size() == 3
		spans.get(0) == s1
		spans.get(1) == s2
		spans.get(2) == s3
	}
}
