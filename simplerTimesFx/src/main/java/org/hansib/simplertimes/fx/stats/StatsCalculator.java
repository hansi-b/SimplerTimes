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
package org.hansib.simplertimes.fx.stats;

import java.time.Duration;
import java.time.LocalDate;
import java.util.HashMap;
import java.util.Map;
import java.util.SortedSet;
import java.util.stream.Collectors;

import org.hansib.simplertimes.fx.data.FxProject;
import org.hansib.simplertimes.fx.data.FxSpan;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

class StatsCalculator {
	private final ObservableList<FxSpan> spans;

	StatsCalculator(ObservableList<FxSpan> spans) {
		this.spans = spans;
	}

	ObservableList<StatsRow> calcItems(SortedSet<LocalDate> dates) {

		Map<FxProject, Map<LocalDate, Duration>> durationsByProject = aggregateStats(dates);

		return durationsByProject.entrySet().stream().map(e -> StatsRow.of(e.getKey(), e.getValue()))
				.collect(Collectors.toCollection(FXCollections::observableArrayList));
	}

	private Map<FxProject, Map<LocalDate, Duration>> aggregateStats(SortedSet<LocalDate> dates) {
		Map<FxProject, Map<LocalDate, Duration>> durationsByProject = new HashMap<>();
		spans.stream().filter(s -> dates.contains(s.start().get().toLocalDate())) //
				.forEach(s -> durationsByProject.computeIfAbsent(s.fxProject().get(), x -> new HashMap<>()).compute(
						s.start().get().toLocalDate(),
						(k, oldV) -> s.duration().get().plus(oldV == null ? Duration.ZERO : oldV)));
		return durationsByProject;
	}
}