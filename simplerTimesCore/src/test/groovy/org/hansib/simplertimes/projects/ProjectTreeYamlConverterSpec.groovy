package org.hansib.simplertimes.projects;

import org.hansib.simplertimes.projects.Project
import org.hansib.simplertimes.projects.ProjectTree
import org.hansib.simplertimes.projects.ProjectTreeYamlConverter

import spock.lang.Specification

public class ProjectTreeYamlConverterSpec extends Specification {

	def 'can convert to yaml'() {

		given:
		def tree = ProjectTree<Project>.root()
		tree.setProject(new Project("root"))
		def book = tree.add(new Project("book"))
		book.add(new Project("chapter 1"))
		book.add(new Project("chapter 2"))
		tree.add(new Project("code"))

		when:
		def yaml = new ProjectTreeYamlConverter().toYaml(tree)

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
		ProjectTree root = new ProjectTreeYamlConverter().fromYaml(yaml)

		then:
		root.project().name() == 'root'
		def c = root.children()
		c.size() == 2
	}

	def 'can modify after converting from yaml'() {

		given:
		def yaml = '''---
project:
  name: "root"
children:
- project:
    name: "book"
  children: []
'''
		when:
		ProjectTree root = new ProjectTreeYamlConverter().fromYaml(yaml)
		root.add(new Project("hello"))

		then:
		root.children.size() == 2
	}
}
