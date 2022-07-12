package org.hansib.simplertimes.times;

import java.time.Duration;
import java.time.ZonedDateTime;

public class Timer {

	private final ZonedDateTime startedAt;

	Timer() {
		startedAt = ZonedDateTime.now();
	}

	public static Timer start() {
		return new Timer();
	}

	public Duration duration() {
		return Duration.between(startedAt, ZonedDateTime.now());
	}

	public Interval interval() {
		return new Interval(startedAt, ZonedDateTime.now());
	}
}
