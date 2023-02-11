package org.hansib.simplertimes.times;

import java.time.Duration;
import java.time.ZonedDateTime;
import java.util.Objects;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.TimeUnit;
import java.util.function.Consumer;

import org.hansib.sundries.Errors;

public class DurationTicker {
	private static class DaemonFactory implements ThreadFactory {
		@Override
		public Thread newThread(Runnable r) {
			Thread t = new Thread(r);
			t.setDaemon(true);
			return t;
		}
	}

	private final ScheduledExecutorService scheduler = Executors.newScheduledThreadPool(0, new DaemonFactory());
	private ScheduledFuture<?> scheduleAtFixedRate;

	private final Consumer<Duration> durationReceiver;

	private ZonedDateTime startedAt;

	public DurationTicker(Consumer<Duration> tickReceiver) {
		this.durationReceiver = Objects.requireNonNull(tickReceiver);
	}

	public synchronized void start() {
		if (startedAt != null)
			throw Errors.illegalState("Ticker was already started");
		startedAt = ZonedDateTime.now();
		scheduleAtFixedRate = scheduler.scheduleAtFixedRate(this::updateTime, 0, 40, TimeUnit.MILLISECONDS);
	}

	private void updateTime() {
		durationReceiver.accept(Duration.between(startedAt, ZonedDateTime.now()));
	}

	public synchronized Interval stopAndGet() {
		if (scheduleAtFixedRate == null || startedAt == null)
			throw Errors.illegalState("Ticker was not started");
		scheduleAtFixedRate.cancel(true);
		Interval result = new Interval(startedAt, ZonedDateTime.now());
		startedAt = null;
		scheduleAtFixedRate = null;
		return result;
	}
}