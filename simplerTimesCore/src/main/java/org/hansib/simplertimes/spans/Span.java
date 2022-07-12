package org.hansib.simplertimes.spans;

import java.time.OffsetDateTime;
import java.time.ZonedDateTime;
import java.time.temporal.ChronoUnit;
import java.util.Objects;

import org.hansib.simplertimes.projects.Project;
import org.hansib.sundries.Errors;

public record Span(Project project, OffsetDateTime start, OffsetDateTime end) {

	private static final int roundingNanos = 1000000000 / 2;

	public Span {
		Objects.requireNonNull(project);
		Objects.requireNonNull(start);
		Objects.requireNonNull(end);

		if (start.compareTo(end) >= 0)
			throw Errors.illegalArg("End (%s) must be after start (%s)", end, start);
	}

	public Span(Project project, ZonedDateTime start, ZonedDateTime end) {
		this(project, round(start), round(end));
	}

	private static OffsetDateTime round(ZonedDateTime ldt) {
		return ldt.plusNanos(roundingNanos).truncatedTo(ChronoUnit.SECONDS).toOffsetDateTime();
	}
}