package org.hansib.simplertimes.fx.data;

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
	
	def 'can format name'() {

		given:
		def p = root.add('project')
		def s1 = p.add('hello')
		def s2 = s1.add('world')

		when:
		def fx = FxProject.root(root)
		def pFx = children(fx)[0]
		def s2Fx = children(children(pFx)[0])[0]
		
		then:
		s2Fx.formatName(pFx, " - ") == "hello - world"
	}

	def 'can get flat list'() {

		given:
		def c1 = root.add('1')
		c1.add('1b')
		root.add('2')

		when:
		def fx = FxProject.root(root)

		then:
		def c = children(fx)
		fx.flatList() == [
			c[0],
			children(c[0])[0],
			c[1]
		]
	}

	def 'can get leaves'() {

		given:
		def c1 = root.add('1')
		c1.add('1b')
		root.add('2x')

		when:
		def fx = FxProject.root(root)

		then:
		def c = children(fx)
		fx.leafChildren().toList() == [
			children(c[0])[0], // 1b
			c[1] // 2x
		]
	}

	def 'leaves() w/o children returns empty list'() {

		given:
		root.add('1')

		when:
		def fx = FxProject.root(root)

		then:
		children(fx)[0].leafChildren().toList() == []
	}
	
	static List<FxProject> children(FxProject p) {
		p.children().toList()
	}
}
