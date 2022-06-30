package org.hansib.simplertimes.projects;

import java.io.IOException;

import org.hansib.simplertimes.yaml.YamlMapper;

public class ProjectYamlConverter {

	private final YamlMapper mapper;

	public ProjectYamlConverter() {
		mapper = YamlMapper.instance();
	}

	public Project fromYaml(String yamlString) throws IOException {
		return mapper.fromString(yamlString, Project.Builder.class).build();
	}

	public String toYaml(Project tree) throws IOException {
		return mapper.asString(tree);
	}
}
