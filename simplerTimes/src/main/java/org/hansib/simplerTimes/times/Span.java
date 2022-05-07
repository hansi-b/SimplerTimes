package org.hansib.simplerTimes.times;

import java.time.LocalDateTime;
import java.util.Objects;

import org.hansib.sundries.Errors;

public record Span(LocalDateTime start, LocalDateTime end) {

	public Span {
		Objects.requireNonNull(start);
		Objects.requireNonNull(end);

		if (start.compareTo(end) > 0)
			throw Errors.illegalArg("Start (%s) cannot be after end (%s)");
	}
}
