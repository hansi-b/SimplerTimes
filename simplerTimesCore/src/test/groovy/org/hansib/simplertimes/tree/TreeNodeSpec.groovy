package org.hansib.simplertimes.tree;

import spock.lang.Specification

public class TreeNodeSpec extends Specification {

	def "can get root"() {

		when:
		def n = TreeNode.root()

		then:
		n.element() == null
		n.parent() == null
	}

	def "can add child"() {

		given:
		def n = TreeNode.<Nameable>root()

		when:
		def m = n.add(new Nameable("hello"))

		then:
		n.children() == [m]

		when:
		def o = n.add(new Nameable("world"))

		then:
		n.children() == [m, o]
	}

	def "can remove child"() {

		given:
		def n = TreeNode.<Nameable>root()
		def m = n.add(new Nameable("hello"))
		def o = n.add(new Nameable("world"))

		when:
		def m2 = n.remove(m)

		then:
		m2 == m
		n.children() == [o]
	}

	def "unkonwn child throws exception"() {

		given:
		def n = TreeNode.<Nameable>root()
		def m = n.add(new Nameable("hello"))
		def o = TreeNode.<Nameable>root()

		when:
		n.remove(o)

		then:
		thrown IllegalArgumentException
	}
}
