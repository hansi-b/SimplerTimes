package org.hansib.simpler_times.times;

import java.time.Duration;
import java.time.LocalDateTime;

public class Timer {

	private final LocalDateTime startedAt;

	private Timer() {
		startedAt = LocalDateTime.now();
	}

	public static Timer start() {
		return new Timer();
	}

	public Duration duration() {
		return Duration.between(startedAt, LocalDateTime.now());
	}

	public Interval interval() {
		return new Interval(startedAt, LocalDateTime.now());
	}
}
