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
package org.hansib.simplertimes.fx.stats;

import java.time.Duration;
import java.time.LocalDate;
import java.util.HashMap;
import java.util.Map;

import org.hansib.simplertimes.fx.data.FxProject;
import org.hansib.simplertimes.times.Utils;

import javafx.beans.property.ReadOnlyStringProperty;
import javafx.beans.property.ReadOnlyStringWrapper;

record StatsRow(ReadOnlyStringProperty project, Map<LocalDate, ReadOnlyStringProperty> durations) {

	static StatsRow of(FxProject p, Map<LocalDate, Duration> durations) {
		Map<LocalDate, ReadOnlyStringProperty> res = new HashMap<>();
		durations.forEach((odt, d) -> res.put(odt, new ReadOnlyStringWrapper(Utils.toHmsString(d))));
		return new StatsRow(p.name(), res);
	}

	ReadOnlyStringProperty ldStr(LocalDate odt) {
		return durations.get(odt);
	}

	public boolean equalsByValues(StatsRow other) {
		if (this == other)
			return true;
		return other != null && //
				project.get().equals(other.project.get()) && //
				durations.size() == other.durations.size() && //
				durations.entrySet().stream().allMatch(e -> {
					ReadOnlyStringProperty otherProp = other.durations.get(e.getKey());
					return otherProp != null && otherProp.get().equals(e.getValue().get());
				});
	}
}
