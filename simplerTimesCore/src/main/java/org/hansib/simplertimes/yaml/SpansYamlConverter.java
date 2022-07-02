package org.hansib.simplertimes.yaml;

import java.io.IOException;

import org.hansib.simplertimes.spans.SpansCollection;

class SpansYamlConverter {

	private SpansYamlConverter() {
		// nothing to instantiate yet
	}

	static SpansCollection fromYaml(String yamlString) throws IOException {
		return YamlMapper.instance().fromString(yamlString, SpansCollection.class);
	}

	static String toYaml(SpansCollection spans) throws IOException {
		return YamlMapper.instance().asString(spans);
	}
}
