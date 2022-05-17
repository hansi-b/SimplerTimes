package org.hansib.simplertimes.yaml;

import java.io.IOException;
import java.time.OffsetDateTime;
import java.time.format.DateTimeFormatter;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.Version;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.ObjectMapper;
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
}
