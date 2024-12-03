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

import java.time.DayOfWeek;
import java.time.Duration;
import java.time.LocalDate;
import java.time.temporal.TemporalAdjusters;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.stream.Collectors;

public class Utils {

	public static String toHmsString(Duration duration) {
		return String.format("%d:%02d:%02d", duration.toHours(), duration.toMinutesPart(), duration.toSecondsPart());
	}

	/**
	 * @return a set of seven dates, starting from the last/current Monday to the
	 *         current/next Sunday
	 */
	public static SortedSet<LocalDate> daysOfCurrentWeek() {
		return daysOfWeek(LocalDate.now());
	}

	/**
	 * Construct a set of the seven days of the current week.
	 * 
	 * @return a set of seven dates, starting from the previous (or current) Monday
	 *         to the next Sunday
	 */
	public static SortedSet<LocalDate> daysOfWeek(final LocalDate day) {
		LocalDate current = day.getDayOfWeek() == DayOfWeek.MONDAY ? day
				: day.with(TemporalAdjusters.previous(DayOfWeek.MONDAY));
		return current.datesUntil(current.plusDays(7)).collect(Collectors.toCollection(TreeSet::new));
	}
}
