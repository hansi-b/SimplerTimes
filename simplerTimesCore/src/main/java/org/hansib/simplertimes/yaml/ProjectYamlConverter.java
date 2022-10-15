package org.hansib.simplertimes.yaml;

import java.io.IOException;

import org.hansib.simplertimes.projects.Project;

public class ProjectYamlConverter {

	private final YamlMapper mapper;

	public ProjectYamlConverter() {
		mapper = YamlMapper.instance();
	}

	public Project fromYaml(String yamlString) throws IOException {
		return mapper.fromString(yamlString, Project.BottomUpBuilder.class).build();
	}

	public String toYaml(Project tree) throws IOException {
		return mapper.asString(tree);
	}
}
