package org.hansib.simplertimes.tree;

import org.hansib.simplertimes.Project

import spock.lang.Specification

public class TreeNodeYamlConverterSpec extends Specification {

	def 'can convert to yaml'() {

		given:
		def tree = TreeNode<Project>.root()
		tree.setElement(new Project("root"))
		def book = tree.add(new Project("book"))
		book.add(new Project("chapter 1"))
		book.add(new Project("chapter 2"))
		tree.add(new Project("code"))

		when:
		def yaml = new TreeNodeYamlConverter().toYaml(tree)

		then:
		yaml == '''---
project:
  name: "root"
children:
- project:
    name: "book"
  children:
  - project:
      name: "chapter 1"
    children: []
  - project:
      name: "chapter 2"
    children: []
- project:
    name: "code"
  children: []
'''
	}

	def 'can convert from yaml'() {

		given:
		def yaml = '''---
project:
  name: "root"
children:
- project:
    name: "book"
  children:
  - project:
      name: "chapter 1"
    children: []
  - project:
      name: "chapter 2"
    children: []
- project:
    name: "code"
  children: []
'''
		when:
		TreeNode<Project> root = new TreeNodeYamlConverter().fromYaml(yaml)

		then:
		root.element().name() == 'root'
		def c = root.children()
		c.size() == 2
	}
}
