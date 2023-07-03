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
package org.hansib.simplertimes.fx;

import java.time.Duration;
import java.time.LocalDate;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.SortedSet;
import java.util.stream.Collectors;

import org.hansib.simplertimes.projects.Project;

import javafx.beans.binding.Bindings;
import javafx.beans.binding.ObjectBinding;
import javafx.collections.ObservableList;

class StatsCalculator {
	private final ObservableList<FxSpan> spans;

	private final ObjectBinding<Set<Project>> projects;

	StatsCalculator(ObservableList<FxSpan> spans) {
		this.spans = spans;

		projects = Bindings.createObjectBinding(
				() -> spans.stream().map(s -> s.project().get()).collect(Collectors.toSet()), spans);
	}

	Set<Project> allProjects() {
		return projects.get();
	}

	Map<LocalDate, Duration> durationsByDate(Project p, SortedSet<LocalDate> dates) {
		Map<LocalDate, Duration> result = new HashMap<>();
		spans.stream() //
				.filter(s -> s.project().get() == p && dates.contains(s.start().get().toLocalDate())) //
				.forEach(s -> result.compute(s.start().get().toLocalDate(),
						(k, oldV) -> s.duration().get().plus(oldV == null ? Duration.ZERO : oldV)));
		return result;
	}
}