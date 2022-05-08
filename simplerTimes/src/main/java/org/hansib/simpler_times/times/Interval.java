package org.hansib.simpler_times.times;

import java.time.LocalDateTime;
import java.util.Objects;

import org.hansib.sundries.Errors;

public record Interval(LocalDateTime start, LocalDateTime end) {

	public Interval {
		Objects.requireNonNull(start);
		Objects.requireNonNull(end);

		if (start.compareTo(end) >= 0)
			throw Errors.illegalArg("End (%s) must be after start (%s)", end, start);
	}
}
