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
import java.time.OffsetDateTime;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hansib.simplertimes.projects.Project;
import org.hansib.simplertimes.times.Utils;
import org.hansib.sundries.fx.AlertBuilder;
import org.hansib.sundries.fx.ContextMenuBuilder;
import org.hansib.sundries.fx.table.EditingCell;
import org.hansib.sundries.fx.table.TableColumnBuilder;

import javafx.beans.binding.DoubleBinding;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.scene.control.Alert.AlertType;
import javafx.scene.control.ButtonType;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableColumn.CellEditEvent;
import javafx.scene.control.TableView;

public class SpansTableController {

	private static final Logger log = LogManager.getLogger();

	@FXML
	private TableColumn<SpanRow, Project> projectCol;

	@FXML
	private TableColumn<SpanRow, OffsetDateTime> startCol;

	@FXML
	private TableColumn<SpanRow, OffsetDateTime> endCol;

	@FXML
	private TableColumn<SpanRow, Duration> durationCol;

	@FXML
	private TableView<SpanRow> spansTable;

	@FXML
	void initialize() {
		log.info("Initialising spans table");

		DateTimeHandler dtHandler = new DateTimeHandler();

		spansTable.setEditable(true);

		new TableColumnBuilder<>(startCol).headerText("Start") //
				.value(SpanRow::start).format(dtHandler.formatter()) //
				.build();
		startCol.setCellFactory(list -> new EditingCell<>(dtHandler.getConverter(), //
				dtHandler::isDateTimeStrValid));

		startCol.setOnEditCommit(this::setNewStart);
		startCol.setEditable(true);

		new TableColumnBuilder<>(endCol).headerText("End") //
				.value(SpanRow::end).format(dtHandler.formatter()) //
				.build();

		endCol.setCellFactory(list -> new EditingCell<>(dtHandler.getConverter(), //
				dtHandler::isDateTimeStrValid));

		endCol.setOnEditCommit(this::setNewEnd);
		endCol.setEditable(true);

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

		spansTable.setContextMenu(new ContextMenuBuilder() //
				.item("Delete", e -> deleteSelected()).build());
	}

	void setSpans(ObservableList<SpanRow> items) {
		spansTable.setItems(items);
		spansTable.getSortOrder().add(startCol);
		spansTable.getSortOrder().add(endCol);
		spansTable.getSortOrder().add(projectCol);
	}

	private void setNewStart(CellEditEvent<SpanRow, OffsetDateTime> e) {
		SpanRow spanRow = e.getTableView().getItems().get(e.getTablePosition().getRow());
		spanRow.start().set(e.getNewValue().withOffsetSameLocal(e.getOldValue().getOffset()));
		spansTable.sort();
	}

	private void setNewEnd(CellEditEvent<SpanRow, OffsetDateTime> e) {
		SpanRow spanRow = e.getTableView().getItems().get(e.getTablePosition().getRow());
		spanRow.end().set(e.getNewValue().withOffsetSameLocal(e.getOldValue().getOffset()));
		spansTable.sort();
	}

	private void deleteSelected() {
		final ObservableList<SpanRow> selectedItems = spansTable.getSelectionModel().getSelectedItems();

		boolean userAgreed = new AlertBuilder(AlertType.WARNING,
				"The deletion of the %d selected item(s) cannot be undone.".formatted(selectedItems.size())) //
						.withDefaultButton(ButtonType.CANCEL, "Cancel") //
						.withButton(ButtonType.YES, "Delete") //
						.showAndWaitFor(ButtonType.YES);

		if (userAgreed)
			spansTable.getItems().removeAll(selectedItems);
	}
}
