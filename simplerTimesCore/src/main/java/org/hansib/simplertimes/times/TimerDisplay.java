package org.hansib.simplertimes.times;

import java.time.Duration;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;

import org.hansib.sundries.Errors;

public class TimerDisplay {
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

	private final AtomicReference<Timer> timerRef;

	private final Consumer<Duration> durationReceiver;

	public TimerDisplay(Consumer<Duration> durationReceiver) {
		this.durationReceiver = durationReceiver;
		this.timerRef = new AtomicReference<>();
	}

	public void start() {
		timerRef.set(Timer.start());
		scheduleAtFixedRate = scheduler.scheduleAtFixedRate(this::updateTime, 0, 40, TimeUnit.MILLISECONDS);
	}

	private void updateTime() {
		Timer timer = timerRef.get();
		if (timer == null)
			return;

		durationReceiver.accept(timer.duration());
	}

	public Interval stopAndGet() {
		Timer timer = timerRef.get();
		if (scheduleAtFixedRate == null || timer == null)
			throw Errors.illegalState("Timer was not started");
		scheduleAtFixedRate.cancel(true);
		return timer.interval();
	}
}