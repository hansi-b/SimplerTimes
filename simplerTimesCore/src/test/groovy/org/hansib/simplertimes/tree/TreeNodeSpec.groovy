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
		def m = n.add(new Project("hello"))

		then:
		n.children() == [m]

		when:
		def o = n.add(new Project("world"))

		then:
		n.children() == [m, o]
	}

	def "can remove child"() {

		given:
		def n = TreeNode.root()
		def m = n.add(new Project("hello"))
		def o = n.add(new Project("world"))

		when:
		def m2 = n.remove(m)

		then:
		m2 == m
		n.children() == [o]
	}

	def "unknown child throws exception"() {

		given:
		def n = TreeNode.root()
		def m = n.add(new Project("hello"))
		def o = TreeNode.root()

		when:
		n.remove(o)

		then:
		thrown IllegalArgumentException
	}

	def "can stream"() {

		given:
		def r = TreeNode.root()
		def m = r.add(new Project("hello"))
		m.add(new Project("a"))
		m.add(new Project("b"))

		def n = r.add(new Project("world"))

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
}
