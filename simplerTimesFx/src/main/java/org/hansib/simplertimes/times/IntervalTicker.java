/**
 * SimplerTimes - https://github.com/hansi-b/SimplerTimes
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

import java.time.ZonedDateTime;
import java.util.Objects;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;

import org.hansib.sundries.Errors;
import org.hansib.sundries.testing.VisibleForTesting;

public class IntervalTicker {
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

	private final DateTimeSource dtSource;
	private final Consumer<Interval> tickReceiver;

	private ZonedDateTime startedAt;
	private final AtomicReference<Interval> lastUpdate;

	public IntervalTicker(Consumer<Interval> tickReceiver) {
		this(tickReceiver, new DateTimeSource.SystemDateTime());
	}

	@VisibleForTesting
	IntervalTicker(Consumer<Interval> tickReceiver, DateTimeSource dateTimeSource) {
		this.tickReceiver = Objects.requireNonNull(tickReceiver);
		this.dtSource = dateTimeSource;
		this.lastUpdate = new AtomicReference<>();
	}

	public synchronized void start() {
		if (startedAt != null)
			throw Errors.illegalState("Ticker was already started");
		startedAt = dtSource.now();
		scheduleAtFixedRate = scheduler.scheduleAtFixedRate(this::updateInterval, 0, 40, TimeUnit.MILLISECONDS);
	}

	private void updateInterval() {
		Interval i = new Interval(startedAt, dtSource.now());
		lastUpdate.set(i);
		tickReceiver.accept(i);
	}

	/**
	 * @return the interval from this ticker's last start to the last time the
	 *         elapsed interval was updated
	 */
	public synchronized Interval lastUpdate() {
		return lastUpdate.get();
	}

	/**
	 * Stops this ticker and returns the interval up to the moment the ticker was
	 * stopped. (Which may be longer than the last update due to the update rate.)
	 * 
	 * @return the interval from the last start up to the moment this method was
	 *         called
	 */
	public synchronized Interval stopAndGet() {
		if (scheduleAtFixedRate == null || startedAt == null)
			throw Errors.illegalState("Ticker was not started");
		scheduleAtFixedRate.cancel(true);
		Interval result = new Interval(startedAt, dtSource.now());
		startedAt = null;
		scheduleAtFixedRate = null;
		return result;
	}
}