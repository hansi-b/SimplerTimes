package org.hansib.simplerTimes.times;

import java.time.Duration;
import java.time.LocalDateTime;

public class Timer {

	private LocalDateTime startedAt;

	public Timer() {
		// nothing to do at the moment
	}

	public synchronized LocalDateTime start() {
		if (startedAt != null)
			throw new IllegalStateException("Timer already started");
		startedAt = LocalDateTime.now();
		return startedAt;
	}

	public Duration currentDuration() {
		return Duration.between(startedAt, LocalDateTime.now());
	}

	public synchronized Span stopAndGet() {
		if (startedAt == null)
			throw new IllegalStateException("Timer not yet started");
		var result = new Span(startedAt, LocalDateTime.now());
		startedAt = null;
		return result;
	}
}
