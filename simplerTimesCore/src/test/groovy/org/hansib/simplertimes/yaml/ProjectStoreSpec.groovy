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
  children:
  - id: 4
    name: "Chapter 4"
    children: []
- id: 77
  name: "77th project"
  children: []
'''

		when:
		def root = new ProjectStore(projYaml.toPath()).load()

		then:
		root.id == 0
		root.name == null

		def (id1, id77) = root.children
		id1.id == 1
		id1.name == 'First one'
		id1.children.size() == 1

		def id4 = id1.children[0]
		id4.id == 4
		id4.name == 'Chapter 4'
		id4.children == []

		id77.id == 77
		id77.name == '77th project'
		id77.children == []
	}

	def 'non-existing file returns empty tree'() {
		given:
		File projYaml = new File(tmpDir.toFile(), 'doesnotexist.yaml')

		when:
		def root = new ProjectStore(projYaml.toPath()).load()

		then:
		root.id == 0
		root.name == null
		root.children.size() == 1

		def newProject = root.children[0]
		newProject.id == 1
		newProject.name == 'New Project'
		newProject.children == []
	}
}
