package org.hansib.simplertimes.fx;

import java.time.Duration;
import java.time.OffsetDateTime;
import java.util.Collection;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hansib.simplertimes.projects.Project;
import org.hansib.simplertimes.spans.Span;
import org.hansib.simplertimes.spans.SpansCollection;

import javafx.beans.binding.Bindings;
import javafx.beans.property.ReadOnlyObjectProperty;
import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.property.SimpleObjectProperty;
import javafx.collections.FXCollections;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;

class ObservableSpans {

	private static final Logger log = LogManager.getLogger();

	static class SpanRow {

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

		@Override
		public String toString() {
			return "[%s -> %s: %s]".formatted(start.get(), end.get(), project.get());
		}
	}

	private final SpansCollection backingSpans;

	private final ObservableList<SpanRow> rows;

	ObservableSpans(SpansCollection spans) {
		this.backingSpans = spans;
		this.rows = FXCollections.observableList(spans.stream().map(SpanRow::new).toList());

		rows.addListener((ListChangeListener<SpanRow>) c -> log.info("Change on {}: {} ", c.getClass(), c));
	}

	ObservableList<SpanRow> getItems() {
		return rows;
	}

	void deleteItems(Collection<SpanRow> items) {
		rows.removeAll(items);
	}

	void setStart(SpanRow spanRow, OffsetDateTime newValue) {
		spanRow.start.set(newValue);
	}
}