package org.hansib.simplertimes.tree;

import org.hansib.simplertimes.tree.Node

import spock.lang.Specification

public class NodeSpec extends Specification {

	def "can get root"() {

		when:
		def n = Node.root()

		then:
		n.element() == null
		n.parent() == null
	}

	def "can add child"() {

		given:
		def n = Node.<String>root()

		when:
		def m = n.add("hello")

		then:
		n.children() == [m]

		when:
		def o = n.add("world")

		then:
		n.children() == [m, o]
	}

	def "can remove child"() {

		given:
		def n = Node.<String>root()
		def m = n.add("hello")
		def o = n.add("world")

		when:
		def m2 = n.remove(m)

		then:
		m2 == m
		n.children() == [o]
	}

	def "unkonwn child throws exception"() {

		given:
		def n = Node.<String>root()
		def m = n.add("hello")
		def o = Node.<String>root()

		when:
		n.remove(o)

		then:
		thrown IllegalArgumentException
	}
}
