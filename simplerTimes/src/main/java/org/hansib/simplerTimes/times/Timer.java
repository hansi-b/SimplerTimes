package org.hansib.simplerTimes.times;

import java.time.LocalDateTime;

public class Timer {

	private final TimesRepo store;

	private LocalDateTime startedAt;

	Timer(TimesRepo store) {
		this.store = store;
	}

	public synchronized void start() {
		if (startedAt != null)
			throw new IllegalStateException("Timer already started");
		startedAt = LocalDateTime.now();
	}

	public synchronized void stop() {
		if (startedAt == null)
			throw new IllegalStateException("Timer not yet started");
		store.addSpan(startedAt, LocalDateTime.now());
		startedAt = null;
	}
}
