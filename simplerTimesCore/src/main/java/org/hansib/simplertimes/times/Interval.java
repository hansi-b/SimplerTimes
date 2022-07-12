package org.hansib.simplertimes.times;

import java.time.ZonedDateTime;
import java.util.Objects;

import org.hansib.sundries.Errors;

public record Interval(ZonedDateTime start, ZonedDateTime end) {

	public Interval {
		Objects.requireNonNull(start);
		Objects.requireNonNull(end);

		if (start.compareTo(end) >= 0)
			throw Errors.illegalArg("End (%s) must be after start (%s)", end, start);
	}
}
