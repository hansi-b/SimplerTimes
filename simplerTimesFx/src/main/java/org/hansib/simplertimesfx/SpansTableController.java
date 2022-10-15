package org.hansib.simplertimesfx;

import static org.hansib.sundries.fx.table.TableViewTools.initDragCellCol;

import java.time.Duration;
import java.time.OffsetDateTime;
import java.time.format.DateTimeFormatter;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hansib.simplertimes.projects.Project;
import org.hansib.simplertimes.spans.Span;
import org.hansib.simplertimes.spans.SpansCollection;

import javafx.beans.property.ObjectProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;

public class SpansTableController {

	private static final Logger log = LogManager.getLogger();

	private static record SpanRow(//
			ObjectProperty<Project> project, //
			ObjectProperty<OffsetDateTime> start, //
			ObjectProperty<OffsetDateTime> end, //
			ObjectProperty<Duration> duration) {

		SpanRow(Span span) {
			this(new SimpleObjectProperty<>(span.project()), new SimpleObjectProperty<>(span.start()),
					new SimpleObjectProperty<>(span.end()),
					new SimpleObjectProperty<>(Duration.between(span.start(), span.end())));
		}
	}

	@FXML
	private TableColumn<SpanRow, Project> projectCol;

	@FXML
	private TableColumn<SpanRow, OffsetDateTime> startCol;

	@FXML
	private TableColumn<SpanRow, OffsetDateTime> endCol;

	@FXML
	private TableColumn<SpanRow, Duration> durationCol;

	private final ObservableList<SpanRow> rows = FXCollections.observableArrayList();

	private final DateTimeFormatter dateTimeFormatter = DateTimeFormatter.ofPattern("dd-MM-yyyy HH:mm:ss");

	@FXML
	TableView<SpanRow> spansTable;

	@FXML
	void initialize() {
		log.info("Initialising spans table");
		spansTable.setItems(rows);
		initDragCellCol(startCol, SpanRow::start, t -> t.format(dateTimeFormatter));
		initDragCellCol(endCol, SpanRow::end, t -> t.format(dateTimeFormatter));
		initDragCellCol(projectCol, SpanRow::project, Project::name);
		initDragCellCol(durationCol, SpanRow::duration, SpansTableController::formatduration);
	}

	static String formatduration(Duration diff) {
		return String.format("%d:%02d:%02d", diff.toHours(), diff.toMinutesPart(), diff.toSecondsPart());
	}

	void setSpans(SpansCollection spans) {
		rows.setAll(spans.stream().map(SpanRow::new).toList());
	}
}
