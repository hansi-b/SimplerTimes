package org.hansib.simplertimes.tree;

import java.io.IOException;

import org.hansib.simplertimes.yaml.YamlMapper;

public class TreeNodeYamlConverter {

	private final YamlMapper mapper;

	public TreeNodeYamlConverter() {
		mapper = YamlMapper.instance();
	}

	@SuppressWarnings("unchecked")
	public TreeNode<Project> fromYaml(String yamlString) throws IOException {
		return mapper.fromString(yamlString, TreeNode.class);
	}

	public String toYaml(TreeNode<Project> tree) throws IOException {
		return mapper.asString(tree);
	}
}
