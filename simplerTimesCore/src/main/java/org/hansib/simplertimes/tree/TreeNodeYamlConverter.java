package org.hansib.simplertimes.tree;

import java.io.IOException;

import org.hansib.simplertimes.Project;
import org.hansib.simplertimes.yaml.YamlMapper;

class TreeNodeYamlConverter {

	private final YamlMapper mapper;

	TreeNodeYamlConverter() {
		mapper = YamlMapper.instance();
	}

	@SuppressWarnings("unchecked")
	TreeNode<Project> fromYaml(String yamlString) throws IOException {
		return mapper.fromString(yamlString, TreeNode.class);
	}

	String toYaml(TreeNode<Project> tree) throws IOException {
		return mapper.asString(tree);
	}
}
