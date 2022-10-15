package org.hansib.simplertimes.projects

import spock.lang.Specification

public class ProjectSpec extends Specification {

	def "can get root"() {

		when:
		def n = Project.root()

		then:
		n.name() == null
		n.parent() == null
	}

	def "can add child"() {

		given:
		def n = Project.root()

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
		def n = Project.root()
		def m = addProject(n, 'hello')
		def o = addProject(n, 'world')

		when:
		def m2 = n.remove(m)

		then:
		m2 == m
		n.children() == [o]
	}

	def "getFullName"() {

		when:
		def n = Project.root()
		def m = addProject(n, 'hello')
		def o = addProject(m, 'world')

		then:
		o.nameWords() == ['hello', 'world']
	}

	def "unknown child throws exception"() {

		given:
		def n = Project.root()
		def m = addProject(n, 'hello')
		def o = Project.root()

		when:
		n.remove(o)

		then:
		thrown IllegalArgumentException
	}

	def "can stream depth-first"() {

		given:
		def r = Project.root()
		// depth 1
		def m = addProject(r, 'hello')
		def n = addProject(r, 'world')
		// depth 2
		addProject(m, 'x')
		addProject(m, 'y')

		addProject(n, 'a')

		when:
		def s = r.dfStream().map(c -> c.name()).toList()

		then:
		s == [
			null,
			'hello',
			'x',
			'y',
			'world',
			'a'
		]
	}

	def "can find by id"() {

		when:
		def r = Project.root()
		def m = addProject(r, 'hello')
		def a = addProject(m, 'a')
		addProject(m, 'b')
		def x = addProject(a, 'x')
		def w = addProject(r, 'world')

		then:
		x == r.findById(x.id())
		w == w.findById(w.id())
		null == w.findById(x.id())
	}

	def "can filter multiple words"() {

		given:
		def r = Project.root()
		addProject(r, 'hello world')
		addProject(r, 'hello mars')

		when:
		def s = r.filter(['he', 'wo'] as Set).map(c -> c.name()).toList()

		then:
		s == ['hello world']
	}

	def "can filter multiple words from subprojects"() {

		given:
		def r = Project.root()
		def hello = addProject(r, 'hello')

		addProject(hello, 'world')
		addProject(hello, 'mars')

		when:
		def s = r.filter(['he', 'wo'] as Set).map(c -> c.name()).toList()

		then:
		s == ['world']
	}

	def "filter returns all children"() {

		given:
		def r = Project.root()
		def m = addProject(r, 'hello')
		def a = addProject(m, 'a')
		addProject(a, 'z')
		addProject(m, 'b')

		addProject(r, 'world')

		when:
		def s = r.filter(['hello'] as Set).map(c -> c.name()).toList()

		then:
		s == ['hello', 'a', 'z', 'b']
	}

	def addProject(node, name) {
		return node.add(name)
	}
}
