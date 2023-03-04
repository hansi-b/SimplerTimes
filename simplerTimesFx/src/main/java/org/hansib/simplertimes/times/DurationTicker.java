/**
 * SimplerTimes - https://github.com/hansi-b/SimplerTimesFx
 *
 * Copyright (C) 2022-2023 Hans Bering
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
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