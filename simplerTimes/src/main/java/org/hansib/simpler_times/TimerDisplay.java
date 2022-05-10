package org.hansib.simpler_times;

import java.time.Duration;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.TimeUnit;

import org.hansib.simpler_times.times.Interval;
import org.hansib.simpler_times.times.Timer;
import org.hansib.sundries.Errors;

import javafx.application.Platform;
import javafx.scene.control.Label;

class TimerDisplay {
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

	private final Timer timer;

	private final Label elapsedTime;

	TimerDisplay(Label timeLabel) {
		this.elapsedTime = timeLabel;
		this.timer = new Timer();
	}

	synchronized void start() {
		timer.start();
		scheduleAtFixedRate = scheduler.scheduleAtFixedRate(() -> Platform.runLater(() -> updateTime()), 0, 40,
				TimeUnit.MILLISECONDS);
	}

	private void updateTime() {
		Duration duration = timer.currentDuration();
		if (duration == null)
			return;
		elapsedTime.setText(
				String.format("%d:%02d:%02d", duration.toHours(), duration.toMinutesPart(), duration.toSecondsPart()));
	}

	synchronized Interval stopAndGet() {
		if (scheduleAtFixedRate == null)
			throw Errors.illegalState("Timer was not started");
		scheduleAtFixedRate.cancel(true);
		return timer.stopAndGet();
	}
}