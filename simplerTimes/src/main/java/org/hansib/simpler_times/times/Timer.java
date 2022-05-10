package org.hansib.simpler_times.times;

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

	public synchronized Duration currentDuration() {
		if (startedAt == null)
			return null;
		return Duration.between(startedAt, LocalDateTime.now());
	}

	public synchronized Interval stopAndGet() {
		if (startedAt == null)
			throw new IllegalStateException("Timer not yet started");
		var result = new Interval(startedAt, LocalDateTime.now());
		startedAt = null;
		return result;
	}
}
