package org.hansib.simplertimes.yaml;

import spock.lang.Specification
import spock.lang.TempDir

public class ProjectStoreSpec extends Specification {

	@TempDir
	def tmpDir

	def 'can read regular YAML'() {

		given:
		File projYaml = new File(tmpDir.toFile(), 'proj.yaml')
		projYaml << '''---
id: 0
name: null
children:
- id: 1
  name: "First one"
  children: []
'''

		when:
		def root = new ProjectStore(projYaml.toPath(), 'My Project').load()

		then:
		root.id == 0
		root.name == null
		root.children.size() == 1

		def id1 = root.children[0]
		id1.id == 1
		id1.name == 'First one'
		id1.children == []
	}

	def 'non-existing file returns empty tree'() {
		given:
		File projYaml = new File(tmpDir.toFile(), 'doesnotexist.yaml')

		when:
		def root = new ProjectStore(projYaml.toPath(), 'My Project').load()

		then:
		root.id == 0
		root.name == null
		root.children.size() == 1

		def newProject = root.children[0]
		newProject.id == 1
		newProject.name == 'My Project'
		newProject.children == []
	}
}
