package org.hansib.simplertimes.projects;

import java.io.IOException;

import org.hansib.simplertimes.yaml.YamlMapper;

public class ProjectTreeYamlConverter {

	private final YamlMapper mapper;

	public ProjectTreeYamlConverter() {
		mapper = YamlMapper.instance();
	}

	public ProjectTree fromYaml(String yamlString) throws IOException {
		return mapper.fromString(yamlString, ProjectTree.class);
	}

	public String toYaml(ProjectTree tree) throws IOException {
		return mapper.asString(tree);
	}
}
