package org.hansib.simplertimes.fx;

import java.time.OffsetDateTime;
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

	SortedSet<OffsetDateTime> allDates() {
		return spansCollection.stream().map(Span::start).collect(Collectors.toCollection(TreeSet::new));
	}
}