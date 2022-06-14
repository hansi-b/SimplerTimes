package org.hansib.simplertimes.tree;

import spock.lang.Specification

public class TreeNodeSpec extends Specification {

	def "can get root"() {

		when:
		def n = TreeNode.root()

		then:
		n.project() == null
		n.parent() == null
	}

	def "can add child"() {

		given:
		def n = TreeNode.root()

		when:
		def m = addProject(n, 'hello')

		then:
		n.children() == [m]

		when:
		def o = addProject(n, 'world')

		then:
		n.children() == [m, o]
	}

	def "can remove child"() {

		given:
		def n = TreeNode.root()
		def m = addProject(n, 'hello')
		def o = addProject(n, 'world')

		when:
		def m2 = n.remove(m)

		then:
		m2 == m
		n.children() == [o]
	}

	def "unknown child throws exception"() {

		given:
		def n = TreeNode.root()
		def m = addProject(n, 'hello')
		def o = TreeNode.root()

		when:
		n.remove(o)

		then:
		thrown IllegalArgumentException
	}

	def "can stream"() {

		given:
		def r = TreeNode.root()
		def m = addProject(r, 'hello')
		addProject(m, 'a')
		addProject(m, 'b')

		addProject(r, 'world')

		when:
		def s = r.dfStream().map(c -> c.project() != null ? c.project().name : null ).toList()

		then:
		s == [
			null,
			'hello',
			'a',
			'b',
			'world'
		]
	}

	def "can filter multiple words"() {

		given:
		def r = TreeNode.root()
		addProject(r, 'hello world')
		addProject(r, 'hello mars')

		when:
		def s = r.filter(['he', 'wo'] as Set).map(c -> c.project() != null ? c.project().name : null ).toList()

		then:
		s == ['hello world']
	}

	def "can filter multiple words from subprojects"() {

		given:
		def r = TreeNode.root()
		def hello = addProject(r, 'hello')

		addProject(hello, 'world')
		addProject(hello, 'mars')

		when:
		def s = r.filter(['he', 'wo'] as Set).map(c -> c.project() != null ? c.project().name : null ).toList()

		then:
		s == ['world']
	}

	def "filter returns all children"() {

		given:
		def r = TreeNode.root()
		def m = addProject(r, 'hello')
		def a = addProject(m, 'a')
		addProject(a, 'z')
		addProject(m, 'b')

		addProject(r, 'world')

		when:
		def s = r.filter(['hello'] as Set).map(c -> c.project() != null ? c.project().name : null ).toList()

		then:
		s == ['hello', 'a', 'z', 'b']
	}

	def addProject(node, name) {
		return node.add(new Project(name))
	}
}
