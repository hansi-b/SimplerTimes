package org.hansib.simplertimes.times;

import java.time.ZonedDateTime;
import java.time.temporal.TemporalAmount;

interface DateTimeSource {

	ZonedDateTime now();

	static class SystemDateTime implements DateTimeSource {
		@Override
		public ZonedDateTime now() {
			return ZonedDateTime.now();
		}
	}

	/**
	 * A "manual" clock which has to be set programmatically - used for tests.
	 */
	static class ManualDateTime implements DateTimeSource {

		private ZonedDateTime now;

		ManualDateTime() {
			setToNow();
		}

		@Override
		public ZonedDateTime now() {
			return now;
		}

		ZonedDateTime setToNow() {
			now = ZonedDateTime.now();
			return now;
		}

		ZonedDateTime turnForward(TemporalAmount amount) {
			now = now.plus(amount);
			return now;
		}
	}
}