package org.hansib.simpler_times;

import java.time.LocalDateTime;

import org.hansib.simpler_times.times.Interval;

record Span(String label, LocalDateTime start, LocalDateTime end) {
	static Span of(String label, Interval span) {
		return new Span(label, cut(span.start()), cut(span.end()));
	}

	private static LocalDateTime cut(LocalDateTime ldt) {
		return ldt.withNano(0);
	}
}