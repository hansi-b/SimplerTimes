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

import java.time.ZonedDateTime;
import java.util.Objects;

import org.hansib.sundries.Errors;

public record Interval(ZonedDateTime start, ZonedDateTime end) {

	public Interval {
		Objects.requireNonNull(start);
		Objects.requireNonNull(end);

		if (start.compareTo(end) >= 0)
			throw Errors.illegalArg("End (%s) must be after start (%s)", end, start);
	}
}
