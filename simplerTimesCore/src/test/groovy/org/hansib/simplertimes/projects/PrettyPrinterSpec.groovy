package org.hansib.simplertimes.projects;

import org.hansib.simplertimes.projects.PrettyPrinter
import org.hansib.simplertimes.projects.Project
import org.hansib.simplertimes.projects.ProjectTree

import spock.lang.Specification

public class PrettyPrinterSpec extends Specification {

	def "can stringify root"() {
		when:
		def n = ProjectTree.root()
		n.add(new Project("xyz"))

		then:
		new PrettyPrinter(n).toPrettyString() == "null\n   xyz\n"
	}
}
