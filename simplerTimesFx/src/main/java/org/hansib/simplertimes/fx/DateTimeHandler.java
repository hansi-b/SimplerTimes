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
package org.hansib.simplertimes.fx;

import java.time.LocalDateTime;
import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.function.Function;

import javafx.util.StringConverter;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hansib.sundries.fx.Converters;

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
			log.warn("Could not parse new value as date: '{}'", text);
			return null;
		}
	}
}