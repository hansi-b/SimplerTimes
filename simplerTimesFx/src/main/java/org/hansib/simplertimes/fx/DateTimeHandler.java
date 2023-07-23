package org.hansib.simplertimes.fx;

import java.time.LocalDateTime;
import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.function.Function;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hansib.sundries.fx.Converters;

import javafx.util.StringConverter;

class DateTimeHandler {

	private static final Logger log = LogManager.getLogger();

	private static final ZoneOffset offset = OffsetDateTime.now().getOffset();
	private static final DateTimeFormatter dateTimeFormatter = DateTimeFormatter.ofPattern("dd-MM-yyyy HH:mm:ss");

	private static final Converters converters = new Converters();

	Function<OffsetDateTime, String> formatter() {
		return t -> t.format(dateTimeFormatter);
	}

	StringConverter<OffsetDateTime> getConverter() {
		return converters.stringConverter( //
				d -> d == null ? "null" : d.format(dateTimeFormatter), this::parseToOffsetDateTime);
	}

	OffsetDateTime parseToOffsetDateTime(String text) {
		try {
			return OffsetDateTime.of(LocalDateTime.parse(text, dateTimeFormatter), offset);
		} catch (DateTimeParseException ex) {
			log.debug("Could not parse new value as date: {}", text);
			return null;
		}
	}
}