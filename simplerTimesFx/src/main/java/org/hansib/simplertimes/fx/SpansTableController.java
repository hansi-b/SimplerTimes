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
import java.time.LocalDateTime;
import java.time.OffsetDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hansib.simplertimes.projects.Project;
import org.hansib.simplertimes.spans.Span;
import org.hansib.simplertimes.spans.SpansCollection;
import org.hansib.simplertimes.times.Utils;
import org.hansib.sundries.fx.AlertBuilder;
import org.hansib.sundries.fx.ContextMenuBuilder;
import org.hansib.sundries.fx.Converters;
import org.hansib.sundries.fx.table.EditingCell;
import org.hansib.sundries.fx.table.TableColumnBuilder;

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
import javafx.scene.control.TableColumn.CellEditEvent;
import javafx.scene.control.TableView;

public class SpansTableController {

	private static final Logger log = LogManager.getLogger();

	private static final Converters converters = new Converters();

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

		spansTable.setItems(rows);
		spansTable.setEditable(true);

		new TableColumnBuilder<>(startCol).headerText("Start") //
				.value(SpanRow::start).format(t -> t.format(dateTimeFormatter)) //
				.build();
		startCol.setCellFactory(list -> new EditingCell<>(converters.stringConverter( //
				d -> d == null ? "null" : dateTimeFormatter.format(d), //
				string -> {
					try {
						return OffsetDateTime.of(LocalDateTime.parse(string, dateTimeFormatter),
								OffsetDateTime.now().getOffset());
					} catch (DateTimeParseException ex) {
						log.debug("Could not parse new value as date: {}", string);
						return null;
					}
				})));

		startCol.setOnEditCommit((CellEditEvent<SpanRow, OffsetDateTime> e) -> {
			SpanRow spanRow = e.getTableView().getItems().get(e.getTablePosition().getRow());
			log.info(">>>>>>> {} --> {}", e.getOldValue(), e.getNewValue());
			Span span = spanRow.span();
			spans.remove(span);
			spans.add(new Span(span.project(), e.getNewValue(), span.end()));
		});
		startCol.setEditable(true);

		new TableColumnBuilder<>(endCol).headerText("End") //
				.value(SpanRow::end).format(t -> t.format(dateTimeFormatter)) //
				.build();
		new TableColumnBuilder<>(projectCol).headerText("Project") //
				.value(SpanRow::project).format(Project::name).comparator(Project.nameComparator) //
				.build();
		new TableColumnBuilder<>(durationCol).headerText("Duration") //
				.value(SpanRow::duration).format(Utils::toHmsString) //
				.build();

		startCol.prefWidthProperty().bind(spansTable.widthProperty().multiply(.25));
		endCol.prefWidthProperty().bind(spansTable.widthProperty().multiply(.25));
		projectCol.prefWidthProperty().bind(spansTable.widthProperty().multiply(.35));

		final DoubleBinding colsWidth = projectCol.widthProperty() //
				.add(startCol.widthProperty()).add(endCol.widthProperty()) //
				.multiply(1.03);

		durationCol.prefWidthProperty().bind(spansTable.widthProperty().subtract(colsWidth));

		createContextMenu();
	}

	private void createContextMenu() {
		ContextMenu cm = new ContextMenuBuilder().item("Delete", e -> deleteSelected(spansTable)).build();

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
