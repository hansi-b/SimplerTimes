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

import static org.hansib.sundries.fx.table.TableViewTools.initDragSelectCellCol;
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
import org.hansib.simplertimes.times.Utils;
import org.hansib.sundries.fx.AlertBuilder;
import org.hansib.sundries.fx.ContextMenuBuilder;

import javafx.beans.binding.DoubleBinding;
import javafx.beans.property.ReadOnlyObjectProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.scene.control.Alert.AlertType;
import javafx.scene.control.ButtonType;
import javafx.scene.control.ContextMenu;
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

	private static class SpanRow {

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

		public ReadOnlyObjectProperty<Project> project() {
			return project;
		}

		public ReadOnlyObjectProperty<OffsetDateTime> start() {
			return start;
		}

		public ReadOnlyObjectProperty<OffsetDateTime> end() {
			return end;
		}

		public ReadOnlyObjectProperty<Duration> duration() {
			return duration;
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
	private TableView<SpanRow> spansTable;

	private SpansCollection spans;

	@FXML
	void initialize() {
		log.info("Initialising spans table");

		projectCol.setText("Project");
		startCol.setText("Start");
		endCol.setText("End");
		durationCol.setText("Duration");

		spansTable.setItems(rows);

		initDragSelectCellCol(startCol, SpanRow::start, t -> t.format(dateTimeFormatter));
		initDragSelectCellCol(endCol, SpanRow::end, t -> t.format(dateTimeFormatter));

		initDragSelectCellCol(projectCol, SpanRow::project, Project::name);
		projectCol.setComparator(new ProjectComparator());

		initDragSelectCellCol(durationCol, SpanRow::duration, Utils::toHmsString);

		setPrefWidth(spansTable, startCol, .25);
		setPrefWidth(spansTable, endCol, .25);
		setPrefWidth(spansTable, projectCol, .35);

		final DoubleBinding colsWidth = projectCol.widthProperty()//
				.add(startCol.widthProperty()).add(endCol.widthProperty())//
				.multiply(1.01);

		durationCol.prefWidthProperty().bind(spansTable.widthProperty().subtract(colsWidth));

		createContextMenu();
	}

	private void createContextMenu() {
		ContextMenu cm = new ContextMenuBuilder().withItem("Delete", e -> deleteSelected(spansTable)).build();

		spansTable.setContextMenu(cm);
	}

	private void deleteSelected(TableView<SpanRow> table) {
		final ObservableList<SpanRow> selectedItems = table.getSelectionModel().getSelectedItems();

		boolean userAgreed = new AlertBuilder(AlertType.WARNING,
				"The deletion of the " + selectedItems.size() + " selected item(s) cannot be undone.") //
						.withDefaultButton(ButtonType.CANCEL, "Cancel") //
						.withButton(ButtonType.YES, "Delete") //
						.showAndWaitFor(ButtonType.YES);

		if (userAgreed) {
			selectedItems.forEach(i -> spans.remove(i.span()));
			updateRows();
		}
	}

	void setSpans(SpansCollection spans) {
		this.spans = spans;
		updateRows();
	}

	private void updateRows() {
		rows.setAll(spans.stream().map(SpanRow::new).toList());
	}
}
