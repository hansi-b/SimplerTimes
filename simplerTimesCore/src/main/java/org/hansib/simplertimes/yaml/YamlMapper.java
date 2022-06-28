package org.hansib.simplertimes.yaml;

import java.io.IOException;
import java.time.OffsetDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.List;

import org.hansib.simplertimes.projects.Project;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.Version;
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

		module.addSerializer(new ProjectSerializer());
		module.addDeserializer(Project.class, new ProjectDeserializer());

		return YAMLMapper.builder().addModule(module).build();
	}

	public <T> T fromString(final String yaml, final Class<T> clazz) throws JsonProcessingException {
		return objectMapper.readValue(yaml, clazz);
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

	private static class ProjectSerializer extends JsonSerializer<Project> {

		@Override
		public Class<Project> handledType() {
			return Project.class;
		}

		@Override
		public void serialize(final Project value, final JsonGenerator gen, final SerializerProvider serializers)
				throws IOException {

			gen.writeStartObject();
			gen.writeStringField("name", value.name());
			gen.writeArrayFieldStart("children");
			List<Project> children = value.children();
			for (Project c : children)
				gen.writeObject(c);
			gen.writeEndArray();
			gen.writeEndObject();
		}
	}

	private static class ProjectDeserializer extends JsonDeserializer<Project> {

		private static final ObjectMapper objectMapper = createObjectMapper();

		@Override
		public Project deserialize(final JsonParser p, final DeserializationContext ctxt) throws IOException {
			JsonNode node = p.readValueAsTree();

			JsonNode projectNode = node.get("name");
			String project = objectMapper.treeToValue(projectNode, String.class);

			JsonNode childrenNode = node.get("children");
			ObjectReader childrenReader = objectMapper.readerForArrayOf(Project.class);
			Project[] children = childrenReader.readValue(childrenNode);

			return Project.connected(project, Arrays.asList(children));
		}
	}
}
