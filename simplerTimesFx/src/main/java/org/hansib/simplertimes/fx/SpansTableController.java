/**
 * Abakus - https://github.com/hansi-b/SimplerTimesFx
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

import static org.hansib.sundries.fx.table.TableViewTools.initDragCellCol;
import static org.hansib.sundries.fx.table.TableViewTools.setPrefWidth;

import java.time.Duration;
import java.time.OffsetDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Comparator;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hansib.simplertimes.projects.Project;
import org.hansib.simplertimes.spans.Span;
import org.hansib.simplertimes.spans.SpansCollection;

import javafx.beans.binding.DoubleBinding;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;

public class SpansTableController {

	private static final Logger log = LogManager.getLogger();

	private static class ProjectComparator implements Comparator<Project> {
		@Override
		public int compare(Project p1, Project p2) {
			if (p1.name() == null)
				return -1;
			if (p2.name() == null)
				return 1;
			return p1.name().compareTo(p2.name());
		}

	}

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

		projectCol.setText("Project");
		startCol.setText("Start");
		endCol.setText("End");
		durationCol.setText("Duration");

		spansTable.setItems(rows);
		initDragCellCol(startCol, SpanRow::start, t -> t.format(dateTimeFormatter));
		initDragCellCol(endCol, SpanRow::end, t -> t.format(dateTimeFormatter));

		initDragCellCol(projectCol, SpanRow::project, Project::name);
		projectCol.setComparator(new ProjectComparator());

		initDragCellCol(durationCol, SpanRow::duration, SpansTableController::formatduration);

		setPrefWidth(spansTable, startCol, .25);
		setPrefWidth(spansTable, endCol, .25);
		setPrefWidth(spansTable, projectCol, .35);

		final DoubleBinding colsWidth = projectCol.widthProperty()//
				.add(startCol.widthProperty()).add(endCol.widthProperty())//
				.multiply(1.01);

		durationCol.prefWidthProperty().bind(spansTable.widthProperty().subtract(colsWidth));
	}

	static String formatduration(Duration diff) {
		return String.format("%d:%02d:%02d", diff.toHours(), diff.toMinutesPart(), diff.toSecondsPart());
	}

	void setSpans(SpansCollection spans) {
		rows.setAll(spans.stream().map(SpanRow::new).toList());
	}
}
