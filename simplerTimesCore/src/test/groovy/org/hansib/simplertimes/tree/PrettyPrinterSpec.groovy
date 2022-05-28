package org.hansib.simplertimes.tree;

import spock.lang.Specification

public class PrettyPrinterSpec extends Specification {

	def "can stringify root"() {
		when:
		def n = TreeNode.root()
		n.add(new Nameable("xyz"))

		then:
		new PrettyPrinter(n, e-> e== null? "null" : e.name()).toPrettyString() == "null\n   xyz\n"
	}
}
