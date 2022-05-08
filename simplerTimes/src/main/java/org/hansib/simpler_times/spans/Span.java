package org.hansib.simpler_times.spans;

import java.time.LocalDateTime;
import java.time.OffsetDateTime;
import java.time.ZoneId;

record Span(String label, OffsetDateTime start, OffsetDateTime end) {

	Span(String label, LocalDateTime start, LocalDateTime end) {
		this(label, cut(start), cut(end));
	}

	private static OffsetDateTime cut(LocalDateTime ldt) {
		return ldt.withNano(0).atZone(ZoneId.systemDefault()).toOffsetDateTime();
	}
}