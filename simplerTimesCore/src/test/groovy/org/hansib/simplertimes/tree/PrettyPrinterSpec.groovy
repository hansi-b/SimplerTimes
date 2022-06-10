package org.hansib.simplertimes.tree;

import spock.lang.Specification

public class PrettyPrinterSpec extends Specification {

	def "can stringify root"() {
		when:
		def n = TreeNode.root()
		n.add(new Project("xyz"))

		then:
		new PrettyPrinter(n).toPrettyString() == "null\n   xyz\n"
	}
}
