package org.hansib.simpler_times;

import java.time.LocalDateTime;
import java.time.OffsetDateTime;
import java.time.ZoneId;

import org.hansib.simpler_times.times.Interval;

record Span(String label, OffsetDateTime start, OffsetDateTime end) {

	Span(String label, LocalDateTime start, LocalDateTime end) {
		this(label, cut(start), cut(end));
	}

	static Span of(String label, Interval span) {
		return new Span(label, span.start(), span.end());
	}

	private static OffsetDateTime cut(LocalDateTime ldt) {
		return ldt.withNano(0).atZone(ZoneId.systemDefault()).toOffsetDateTime();
	}
}