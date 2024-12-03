/*-
 * GPL for SimplerTimes - https://github.com/hansi-b/SimplerTimes
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

import java.time.Instant;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.temporal.TemporalAmount;

interface DateTimeSource {

	/**
	 * @return the current time of this date time source
	 */
	ZonedDateTime now();

	static class SystemDateTime implements DateTimeSource {
		@Override
		public ZonedDateTime now() {
			return ZonedDateTime.now();
		}
	}

	/**
	 * A "manual" clock which has to be set programmatically. Used for tests.
	 */
	static class ManualDateTime implements DateTimeSource {

		private ZonedDateTime now;

		ManualDateTime() {
			now = ZonedDateTime.now();
		}

		ManualDateTime(int seconds) {
			now = ZonedDateTime.ofInstant(Instant.ofEpochSecond(seconds), ZoneId.systemDefault());
		}

		@Override
		public ZonedDateTime now() {
			return now;
		}

		ZonedDateTime turnForward(TemporalAmount amount) {
			now = now.plus(amount);
			return now;
		}
	}
}