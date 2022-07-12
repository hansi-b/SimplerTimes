package org.hansib.simplertimes.times;

import java.time.Duration;
import java.time.ZonedDateTime;
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
		this.durationReceiver = tickReceiver;
	}

	public void start() {
		startedAt = ZonedDateTime.now();
		scheduleAtFixedRate = scheduler.scheduleAtFixedRate(this::updateTime, 0, 40, TimeUnit.MILLISECONDS);
	}

	private void updateTime() {
		durationReceiver.accept(Duration.between(startedAt, ZonedDateTime.now()));
	}

	public Interval stopAndGet() {
		if (scheduleAtFixedRate == null || startedAt == null)
			throw Errors.illegalState("Ticker was not started");
		scheduleAtFixedRate.cancel(true);
		return new Interval(startedAt, ZonedDateTime.now());
	}
}