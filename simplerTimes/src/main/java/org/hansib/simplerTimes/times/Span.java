package org.hansib.simplerTimes.times;

import java.time.LocalDateTime;

import org.hansib.sundries.Errors;

public record Span(LocalDateTime start, LocalDateTime end) {

	public Span {
		if (start.compareTo(end) > 0)
			throw Errors.illegalArg("Start (%s) cannot be after end (%s)");
	}
}
