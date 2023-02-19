package org.hansib.simplertimes.yaml;

import org.hansib.simplertimes.projects.Project

import spock.lang.Specification

public class ProjectYamlConverterSpec extends Specification {

	def 'can convert to yaml'() {

		given:
		def tree = Project.root()
		tree.setName("root")
		def book = tree.add("book")
		book.add("chapter 1")
		book.add("chapter 2")
		tree.add("code")

		when:
		def yaml = new ProjectYamlConverter().toYaml(tree)

		then:
		yaml == '''---
id: 0
name: "root"
children:
- id: 1
  name: "book"
  children:
  - id: 2
    name: "chapter 1"
    children: []
  - id: 3
    name: "chapter 2"
    children: []
- id: 4
  name: "code"
  children: []
'''
	}

	def 'can convert from yaml'() {

		given:
		def yaml = '''---
id: 0
name: "root"
children:
- id: 1
  name: "Book"
  children:
  - id: 2
    name: "chapter 1"
    children: []
  - id: 3
    name: "chapter 2"
    children: []
- id: 4
  name: "Code"
  children: []
'''
		when:
		Project root = new ProjectYamlConverter().fromYaml(yaml)

		then:
		root.name() == 'root'
		root.children().size() == 2

		def (id1, id4) = root.children()
		id1.id() == 1
		id1.name() == 'Book'
		id1.children().size() == 2

		id4.id() == 4
		id4.name() == 'Code'
		id4.children() == []

		def (id2, id3) = id1.children()
		id2.id() == 2
		id2.name() == 'chapter 1'
		id2.children() == []

		id3.id() == 3
		id3.name() == 'chapter 2'
		id3.children() == []
	}

	def 'can modify after converting from yaml'() {

		given:
		def yaml = '''---
id: 0
name: "root"
children:
- id: 1
  name: "book"
  children: []
'''
		when:
		Project root = new ProjectYamlConverter().fromYaml(yaml)

		then:
		root.children.size() == 1
		root.children[0].name() == 'book'

		when:
		root.add('ergo')

		then:
		root.children.size() == 2
		root.children[1].id() == 2
		root.children[1].name() == 'ergo'
	}
}
