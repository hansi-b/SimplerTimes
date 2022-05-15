package org.hansib.simpler_times.fx;

import java.time.Duration;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;

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

	private final AtomicReference<Timer> timerRef;

	private final Label elapsedTimeLabel;

	TimerDisplay(Label timeLabel) {
		this.elapsedTimeLabel = timeLabel;
		this.timerRef = new AtomicReference<>();
	}

	void start() {
		timerRef.set(Timer.start());
		scheduleAtFixedRate = scheduler.scheduleAtFixedRate(() -> Platform.runLater(this::updateTimeLabel), 0, 40,
				TimeUnit.MILLISECONDS);
	}

	private void updateTimeLabel() {
		Timer timer = timerRef.get();
		if (timer == null)
			return;

		Duration duration = timer.duration();
		elapsedTimeLabel.setText(
				String.format("%d:%02d:%02d", duration.toHours(), duration.toMinutesPart(), duration.toSecondsPart()));
	}

	Interval stopAndGet() {
		Timer timer = timerRef.get();
		if (scheduleAtFixedRate == null || timer == null)
			throw Errors.illegalState("Timer was not started");
		scheduleAtFixedRate.cancel(true);
		return timer.interval();
	}
}