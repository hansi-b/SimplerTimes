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