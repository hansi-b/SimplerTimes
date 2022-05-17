package org.hansib.simplertimes.spans;

import java.time.LocalDateTime;
import java.time.OffsetDateTime;
import java.time.ZoneId;
import java.time.temporal.ChronoUnit;
import java.util.Objects;

import org.hansib.sundries.Errors;

public record Span(String label, OffsetDateTime start, OffsetDateTime end) {

	private static final int nanosToAdd = 1000000000 / 2;

	public Span {
		Objects.requireNonNull(start);
		Objects.requireNonNull(end);

		if (start.compareTo(end) >= 0)
			throw Errors.illegalArg("End (%s) must be after start (%s)", end, start);
	}

	public Span(String label, LocalDateTime start, LocalDateTime end) {
		this(label, round(start), round(end));
	}

	private static OffsetDateTime round(LocalDateTime ldt) {
		return ldt.plusNanos(nanosToAdd).truncatedTo(ChronoUnit.SECONDS).atZone(ZoneId.systemDefault())
				.toOffsetDateTime();
	}
}