package org.hansib.simplertimes.fx;

import java.time.Duration;
import java.time.OffsetDateTime;

import org.hansib.simplertimes.spans.Span;

import javafx.beans.binding.Bindings;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.ReadOnlyObjectProperty;
import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.property.SimpleObjectProperty;

public class FxSpan {

	private final SimpleObjectProperty<FxProject> fxProject;

	private final SimpleObjectProperty<OffsetDateTime> start;
	private final SimpleObjectProperty<OffsetDateTime> end;
	private final ReadOnlyObjectWrapper<Duration> duration;

	FxSpan(FxProject project, Span span) {
		this.fxProject = new SimpleObjectProperty<>(project);
		this.start = new SimpleObjectProperty<>(span.start());
		this.end = new SimpleObjectProperty<>(span.end());

		this.duration = new ReadOnlyObjectWrapper<>();
		duration.bind(Bindings.createObjectBinding(() -> Duration.between(start.get(), end.get()), start, end));
	}

	public ObjectProperty<FxProject> fxProject() {
		return fxProject;
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
		return new Span(fxProject.get().project(), start.get(), end.get());
	}

	@Override
	public String toString() {
		return "[%s -> %s: %s]".formatted(start.get(), end.get(), fxProject.get());
	}
}