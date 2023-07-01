package org.hansib.simplertimes.fx;

import java.time.Duration;
import java.time.OffsetDateTime;

import org.hansib.simplertimes.projects.Project;
import org.hansib.simplertimes.spans.Span;

import javafx.beans.binding.Bindings;
import javafx.beans.property.ReadOnlyObjectProperty;
import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.property.SimpleObjectProperty;

class SpanRow {

	private final SimpleObjectProperty<Project> project;
	private final SimpleObjectProperty<OffsetDateTime> start;
	private final SimpleObjectProperty<OffsetDateTime> end;
	private final ReadOnlyObjectWrapper<Duration> duration;

	SpanRow(Span span) {
		this.project = new SimpleObjectProperty<>(span.project());
		this.start = new SimpleObjectProperty<>(span.start());
		this.end = new SimpleObjectProperty<>(span.end());

		this.duration = new ReadOnlyObjectWrapper<>();
		duration.bind(Bindings.createObjectBinding(() -> Duration.between(start.get(), end.get()), start, end));
	}

	public SimpleObjectProperty<Project> project() {
		return project;
	}

	public SimpleObjectProperty<OffsetDateTime> start() {
		return start;
	}

	public SimpleObjectProperty<OffsetDateTime> end() {
		return end;
	}

	public ReadOnlyObjectProperty<Duration> duration() {
		return duration;
	}

	public Span toSpan() {
		return new Span(project.get(), start.get(), end.get());
	}

	@Override
	public String toString() {
		return "[%s -> %s: %s]".formatted(start.get(), end.get(), project.get());
	}
}