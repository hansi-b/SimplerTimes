package org.hansib.simplertimes.projects;

import spock.lang.Specification

public class ProjectTreeYamlConverterSpec extends Specification {

	def 'can convert to yaml'() {

		given:
		def tree = ProjectTree.root()
		tree.setProject("root")
		def book = tree.add("book")
		book.add("chapter 1")
		book.add("chapter 2")
		tree.add("code")

		when:
		def yaml = new ProjectTreeYamlConverter().toYaml(tree)

		then:
		yaml == '''---
name: "root"
children:
- name: "book"
  children:
  - name: "chapter 1"
    children: []
  - name: "chapter 2"
    children: []
- name: "code"
  children: []
'''
	}

	def 'can convert from yaml'() {

		given:
		def yaml = '''---
name: "root"
children:
- name: "book"
  children:
  - name: "chapter 1"
    children: []
  - name: "chapter 2"
    children: []
- name: "code"
  children: []
'''
		when:
		ProjectTree root = new ProjectTreeYamlConverter().fromYaml(yaml)

		then:
		root.name() == 'root'
		def c = root.children()
		c.size() == 2
	}

	def 'can modify after converting from yaml'() {

		given:
		def yaml = '''---
name: "root"
children:
- name: "book"
  children: []
- name: "ergo"
  children: []
'''
		when:
		ProjectTree root = new ProjectTreeYamlConverter().fromYaml(yaml)

		then:
		root.children.size() == 2
		root.children[0].name() == "book"
		root.children[1].name() == "ergo"
	}
}
