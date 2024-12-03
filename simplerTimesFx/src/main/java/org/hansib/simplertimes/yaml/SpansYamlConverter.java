/*-
 * GPL for SimplerTimes - https://github.com/hansi-b/SimplerTimes
 *
 * Copyright (C) 2022-2023 Hans Bering
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
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
		return YamlMapper.instance().asString(spans.stream().sorted(Span.startStopProjectComparator).toList());
	}
}
