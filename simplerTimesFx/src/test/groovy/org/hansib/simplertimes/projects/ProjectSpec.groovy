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
		def m = n.add('hello')

		then:
		n.children() == [m]

		when:
		def o = n.add('world')

		then:
		n.children() == [m, o]
	}

	def "can sort children"() {

		given:
		def r = Project.root()

		def x = r.add('xyz')
		def a = r.add('abc')
		def k = r.add('klm')
		def g = r.add('ghi')

		when:
		r.sortChildren((p1, p2) -> p1.name.compareTo(p2.name))

		then:
		r.children() == [a, g, k, x]
	}

	def "can remove child"() {

		given:
		def n = Project.root()
		def m = n.add('hello')
		def o = n.add('world')

		when:
		def m2 = n.remove(m)

		then:
		m2 == m
		n.children() == [o]
	}

	def "removing non-child throws exception"() {

		given:
		def r = Project.root()
		def m = r.add('hello')
		def o = r.add('world')

		when:
		m.remove(o)

		then:
		thrown IllegalArgumentException
	}

	def "move check: root cannot be moved"() {

		given:
		def root = Project.root()
		def c1 = root.add('c1')
		def c2 = root.add('c2')

		expect:
		!root.canMoveTo(c1, 0)
	}

	def "can move node to a sibling" () {

		given:
		def r = Project.root()
		def c1 = r.add('c1')
		def c2 = r.add('c2')

		when:
		c1.moveTo(c2, 0)
		then:
		r.children() == [c2]
		c1.parent() == c2
		c2.children() == [c1]
	}

	def "can move node to an ancestor" () {

		given:
		def r = Project.root()
		def c1 = r.add('c1')
		def c2 = c1.add('c2')
		def c3 = c2.add('c3')

		when:
		c3.moveTo(c1, 1)
		then:
		c1.children() == [c2, c3]
		c3.parent() == c1
		c2.children() == []
	}

	def "move check: child cannot be moved to itself or child or its current position"() {

		given:
		def root = Project.root()
		def c1 = root.add('c1')
		def c2 = c1.add('c2')
		def c3 = c2.add('c3')

		expect:
		!c2.canMoveTo(c1, 0)
		!c2.canMoveTo(c2, 0)
		!c2.canMoveTo(c3, 0)
	}

	def "can move node to different position" () {

		given:
		def r = Project.root()
		def c1 = r.add('c1')
		def c2 = r.add('c2')
		def c3 = r.add('c3')

		when:
		c1.moveTo(r, 2)
		then:
		r.children() == [c2, c3, c1]
		when:
		c1.moveTo(r, 1)
		then:
		r.children() == [c2, c1, c3]
	}

	def "getFullName"() {

		when:
		def n = Project.root()
		def m = n.add('hello')
		def o = m.add('world')

		then:
		o.nameWords() == ['hello', 'world']
	}

	def "unknown child throws exception"() {

		given:
		def n = Project.root()
		def m = n.add('hello')
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
		def m = r.add('hello')
		def n = r.add('world')
		// depth 2
		m.add('x')
		m.add('y')

		n.add('a')

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
		def m = r.add('hello')
		def a = m.add('a')
		m.add('b')
		def x = a.add('x')
		def w = r.add('world')

		then:
		x == r.findById(x.id())
		w == w.findById(w.id())
		null == w.findById(x.id())
	}

	def "can filter multiple words"() {

		given:
		def r = Project.root()
		r.add('hello world')
		r.add('hello mars')

		when:
		def s = r.filter(['he', 'wo'] as Set).map(c -> c.name()).toList()

		then:
		s == ['hello world']
	}

	def "can filter multiple words from subprojects"() {

		given:
		def r = Project.root()
		def hello = r.add('hello')

		hello.add('world')
		hello.add('mars')

		when:
		def s = r.filter(['he', 'wo'] as Set).map(c -> c.name()).toList()

		then:
		s == ['world']
	}

	def "filter returns all children"() {

		given:
		def r = Project.root()
		def m = r.add('hello')
		def a = m.add('a')
		a.add('z')
		m.add('b')

		r.add('world')

		when:
		def s = r.filter(['hello'] as Set).map(c -> c.name()).toList()

		then:
		s == ['hello', 'a', 'z', 'b']
	}

	def 'can build simple tree'() {

		given:
		def root = new Project.Builder(45, 'wired')
		def depth1 = new Project.Builder(7, 'c2')
		def depth2 = new Project.Builder(78, 'xyz')

		root.addChild(depth1)
		depth1.addChild(depth2)

		when:
		Project r = root.build()

		then:
		r.id == 45
		r.name == 'wired'
		r.children.size() == 1

		def d1 = r.children[0]
		d1.id == 7
		d1.name == 'c2'
		d1.children.size() == 1

		def d2 = d1.children[0]
		d2.id == 78
		d2.name == 'xyz'
		d2.children.isEmpty()
	}

	def 'duplicate ids in builder throws error'() {

		given:
		def root = new Project.Builder(7, 'parent')
		def depth1 = new Project.Builder(8, 'child')
		def depth2 = new Project.Builder(7, 'child2')

		root.addChild(depth1)
		depth1.addChild(depth2)

		when:
		root.build()

		then:
		def ex = thrown IllegalArgumentException
		ex.message == "Duplicate id 7: New name 'child2', old 'parent'"
	}
}
