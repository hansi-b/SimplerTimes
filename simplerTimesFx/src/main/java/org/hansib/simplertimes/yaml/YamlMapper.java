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
import java.time.OffsetDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.List;

import org.hansib.simplertimes.projects.Project;
import org.hansib.simplertimes.projects.Project.Builder;
import org.hansib.simplertimes.spans.Span;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.Version;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.ObjectReader;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.fasterxml.jackson.databind.module.SimpleModule;
import com.fasterxml.jackson.dataformat.yaml.YAMLMapper;

public class YamlMapper {

	private static final String FIELD_ID = "id";
	private static final String FIELD_NAME = "name";
	private static final String FIELD_CHILDREN = "children";

	private static final String FIELD_PROJECT_ID = "projectId";
	private static final String FIELD_START = "start";
	private static final String FIELD_END = "end";

	private static final DateTimeFormatter dtFormatter = DateTimeFormatter.ISO_OFFSET_DATE_TIME;

	private static YamlMapper instance;

	public static synchronized YamlMapper instance() {
		if (instance == null)
			instance = new YamlMapper();
		return instance;
	}

	private final ObjectMapper objectMapper;

	private YamlMapper() {
		objectMapper = createObjectMapper();
	}

	private static ObjectMapper createObjectMapper() {

		final SimpleModule module = new SimpleModule("SimplerTimes", new Version(1, 0, 0, "", "", ""));

		module.addSerializer(OffsetDateTime.class, new OffsetDateTimeSerializer());
		module.addDeserializer(OffsetDateTime.class, new OffsetDateTimeDeserializer());

		module.addSerializer(new SpanSerializer());
		module.addDeserializer(SpanStub.class, new SpanStubDeserializer());
		module.addSerializer(new ProjectSerializer());
		module.addDeserializer(Project.Builder.class, new ProjectDeserializer());

		return YAMLMapper.builder().addModule(module).build();
	}

	public <T> T fromString(final String yaml, final Class<T> clazz) throws JsonProcessingException {
		return objectMapper.readValue(yaml, clazz);
	}

	public <T> T fromString(final String yaml, final TypeReference<T> type) throws JsonProcessingException {
		return objectMapper.readValue(yaml, type);
	}

	public <T> String asString(final T model) throws JsonProcessingException {
		return objectMapper.writeValueAsString(model);
	}

	private static class OffsetDateTimeSerializer extends JsonSerializer<OffsetDateTime> {
		@Override
		public void serialize(final OffsetDateTime value, final JsonGenerator gen, final SerializerProvider serializers)
				throws IOException {
			gen.writeString(dtFormatter.format(value));
		}
	}

	private static class OffsetDateTimeDeserializer extends JsonDeserializer<OffsetDateTime> {
		@Override
		public OffsetDateTime deserialize(final JsonParser p, final DeserializationContext ctxt) throws IOException {
			return OffsetDateTime.parse(p.getValueAsString(), dtFormatter);
		}
	}

	private static class SpanSerializer extends JsonSerializer<Span> {
		@Override
		public Class<Span> handledType() {
			return Span.class;
		}

		@Override
		public void serialize(final Span value, final JsonGenerator gen, final SerializerProvider serializers)
				throws IOException {
			gen.writeStartObject();
			gen.writeNumberField(FIELD_PROJECT_ID, value.project().id());
			gen.writeObjectField(FIELD_START, value.start());
			gen.writeObjectField(FIELD_END, value.end());
			gen.writeEndObject();
		}
	}

	private static class SpanStubDeserializer extends JsonDeserializer<SpanStub> {

		private static final ObjectMapper objectMapper = createObjectMapper();

		@Override
		public SpanStub deserialize(final JsonParser p, final DeserializationContext ctxt) throws IOException {
			JsonNode node = p.readValueAsTree();

			Long id = objectMapper.treeToValue(node.get(FIELD_PROJECT_ID), Long.class);
			OffsetDateTime start = objectMapper.treeToValue(node.get(FIELD_START), OffsetDateTime.class);
			OffsetDateTime end = objectMapper.treeToValue(node.get(FIELD_END), OffsetDateTime.class);

			return new SpanStub(id, start, end);
		}
	}

	private static class ProjectSerializer extends JsonSerializer<Project> {

		@Override
		public Class<Project> handledType() {
			return Project.class;
		}

		@Override
		public void serialize(final Project value, final JsonGenerator gen, final SerializerProvider serializers)
				throws IOException {

			gen.writeStartObject();
			gen.writeNumberField(FIELD_ID, value.id());

			gen.writeStringField(FIELD_NAME, value.name());
			gen.writeArrayFieldStart(FIELD_CHILDREN);
			List<Project> children = value.children();
			for (Project c : children)
				gen.writeObject(c);
			gen.writeEndArray();
			gen.writeEndObject();
		}
	}

	private static class ProjectDeserializer extends JsonDeserializer<Project.Builder> {

		private static final ObjectMapper objectMapper = createObjectMapper();

		@Override
		public Project.Builder deserialize(final JsonParser p, final DeserializationContext ctxt) throws IOException {
			JsonNode node = p.readValueAsTree();

			Long id = objectMapper.treeToValue(node.get(FIELD_ID), Long.class);
			String name = objectMapper.treeToValue(node.get(FIELD_NAME), String.class);

			Builder builder = new Project.Builder(id, name);

			ObjectReader childrenReader = objectMapper.readerForArrayOf(Project.Builder.class);

			Project.Builder[] children = childrenReader.readValue(node.get(FIELD_CHILDREN));
			Arrays.stream(children).forEach(builder::addChild);

			return builder;
		}
	}
}
