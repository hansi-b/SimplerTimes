package org.hansib.simplertimes.fx;

import java.time.Duration;
import java.time.OffsetDateTime;
import java.util.Collection;

import org.hansib.simplertimes.projects.Project;
import org.hansib.simplertimes.spans.Span;
import org.hansib.simplertimes.spans.SpansCollection;

import javafx.beans.property.ReadOnlyObjectProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

class SpansTableModel {

	static class SpanRow {

		private final Span span;
		private final SimpleObjectProperty<Project> project;
		private final SimpleObjectProperty<OffsetDateTime> start;
		private final SimpleObjectProperty<OffsetDateTime> end;
		private final ReadOnlyObjectProperty<Duration> duration;

		SpanRow(Span span) {
			this.span = span;
			this.project = new SimpleObjectProperty<>(span.project());
			this.start = new SimpleObjectProperty<>(span.start());
			this.end = new SimpleObjectProperty<>(span.end());
			this.duration = new SimpleObjectProperty<>(span.duration());
		}

		public Span span() {
			return span;
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
	}

	private final SpansCollection spans;

	private final ObservableList<SpanRow> rows = FXCollections.observableArrayList();

	SpansTableModel(SpansCollection spans) {
		this.spans = spans;
		updateRows();
	}

	private void updateRows() {
		rows.setAll(spans.stream().map(SpanRow::new).toList());
	}

	ObservableList<SpanRow> getItems() {
		return rows;
	}

	void deleteItems(Collection<SpanRow> items) {
		items.forEach(i -> spans.remove(i.span()));
		updateRows();
	}

	void setStart(SpanRow spanRow, OffsetDateTime newValue) {
		Span span = spanRow.span();
		spans.remove(span);
		spans.add(new Span(span.project(), newValue, span.end()));
	}
}