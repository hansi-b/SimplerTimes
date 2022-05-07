package org.hansib.simpler_times;

import java.time.Duration;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;

import org.hansib.simpler_times.times.Span;
import org.hansib.simpler_times.times.Timer;
import org.hansib.sundries.Errors;

import javafx.application.Platform;
import javafx.scene.control.Label;

class TimerDisplay {
	private final ScheduledExecutorService scheduler = Executors.newScheduledThreadPool(1);
	private ScheduledFuture<?> scheduleAtFixedRate;

	private final Timer timer;

	private final Label elapsedTime;

	TimerDisplay(Label timeLabel) {
		this.elapsedTime = timeLabel;
		this.timer = new Timer();
	}

	synchronized void start() {
		timer.start();
		scheduleAtFixedRate = scheduler.scheduleAtFixedRate(
				() -> Platform.runLater(() -> elapsedTime.setText(fmtTime(timer.currentDuration()))), 0, 40,
				TimeUnit.MILLISECONDS);
	}

	synchronized Span stopAndGet() {
		if (scheduleAtFixedRate == null)
			throw Errors.illegalState("Timer was not started");
		scheduleAtFixedRate.cancel(true);
		return timer.stopAndGet();
	}

	private static String fmtTime(Duration d) {
		return String.format("%d:%02d:%02d", d.toHours(), d.toMinutesPart(), d.toSecondsPart());
	}
}