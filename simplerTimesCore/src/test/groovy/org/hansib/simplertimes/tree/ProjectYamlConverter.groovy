package org.hansib.simplertimes.tree;

import org.hansib.simplertimes.Project
import org.hansib.simplertimes.yaml.YamlMapper

import spock.lang.Specification

public class ProjectYamlConverter extends Specification {

	def 'can convert to yaml'() {

		given:
		def p = new Project("chapter 1")
		when:
		def yaml = new YamlMapper().asString(p)

		then:
		yaml == '''---
name: "chapter 1"
'''
	}

	def 'can convert from yaml'() {

		given:
		def yaml = '''---
name: "chapter 1"
'''
		when:
		Project p = new YamlMapper().fromString(yaml, Project.class)

		then:
		p.name() == 'chapter 1'
	}
}
