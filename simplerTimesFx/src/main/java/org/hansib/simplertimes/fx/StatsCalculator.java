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
import java.util.TreeSet;
import java.util.stream.Collectors;

import org.hansib.simplertimes.projects.Project;
import org.hansib.simplertimes.spans.Span;
import org.hansib.simplertimes.spans.SpansCollection;

class StatsCalculator {
	private final SpansCollection spansCollection;

	StatsCalculator(SpansCollection spansCollection) {
		this.spansCollection = spansCollection;
	}

	Set<Project> allProjects() {
		return spansCollection.stream().map(Span::project).collect(Collectors.toSet());
	}

	SortedSet<LocalDate> allDates() {
		return spansCollection.stream().map(s -> s.start().toLocalDate())
				.collect(Collectors.toCollection(TreeSet::new));
	}

	public Map<LocalDate, Duration> get(Project p, SortedSet<LocalDate> allDates) {
		Map<LocalDate, Duration> result = new HashMap<>();
		spansCollection.forEach(s -> {
			if (s.project() == p && allDates.contains(s.start().toLocalDate()))
				result.compute(s.start().toLocalDate(),
						(k, oldV) -> s.duration().plus(oldV == null ? Duration.ZERO : oldV));
		});
		return result;
	}
}