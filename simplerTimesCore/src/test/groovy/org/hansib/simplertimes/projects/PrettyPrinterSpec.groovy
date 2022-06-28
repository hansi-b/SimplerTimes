package org.hansib.simplertimes.projects;

import spock.lang.Specification

public class PrettyPrinterSpec extends Specification {

	def "can stringify root"() {
		when:
		def n = ProjectTree.root()
		n.add("xyz")

		then:
		new PrettyPrinter(n).toPrettyString() == "null\n   xyz\n"
	}
}
