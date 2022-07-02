package org.hansib.simplertimes.yaml;

import java.io.IOException;
import java.util.List;

import org.hansib.simplertimes.projects.Project;
import org.hansib.simplertimes.spans.Span;
import org.hansib.simplertimes.spans.SpansCollection;

import com.fasterxml.jackson.core.type.TypeReference;

class SpansYamlConverter {

	private SpansYamlConverter() {
		// nothing to instantiate yet
	}

	static SpansCollection fromYaml(Project root, String yamlString) throws IOException {
		SpansCollection spansCollection = new SpansCollection();
		for (SpanStub s : YamlMapper.instance().fromString(yamlString, new TypeReference<List<SpanStub>>() {
		}))
			spansCollection.add(new Span(root.findById(s.projectId()), s.start(), s.end()));
		return spansCollection;
	}

	static String toYaml(SpansCollection spans) throws IOException {
		return YamlMapper.instance().asString(spans.view());
	}
}
