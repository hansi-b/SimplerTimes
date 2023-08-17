package org.hansib.simplertimes.fx.data;

import org.hansib.simplertimes.fx.data.FxProject
import org.hansib.simplertimes.projects.Project

import spock.lang.Specification

public class FxProjectSpec extends Specification {

	def root = Project.root()

	def 'can create fxProject from project'() {

		given:
		root.add('1')
		root.add('2')

		when:
		def fx = FxProject.root(root)

		then:
		fx.text() == null

		when:
		def children = fx.children() as List

		then:
		children.size() == 2
		children[0].text() == '1'
		children[1].text() == '2'
	}

	def 'can add and remove child'() {

		given:
		def fx = FxProject.root(root)

		when:
		def c = fx.addChild('child1')

		then:
		fx.children() as List == [c]
		root.children().size() == 1
		root.children()[0].name() == 'child1'

		when:
		c.remove()

		then:
		fx.children() as List == []
		root.children().isEmpty()
	}

	def 'can get flat list'() {

		given:
		def c1 = root.add('1')
		root.add('2')
		def c3 = c1.add('1b')

		when:
		def fx = FxProject.root(root)

		then:
		def children = fx.children()
		fx.flatList() == [
			children[0],
			children[0].children()[0],
			children[1]
		]
	}
}
