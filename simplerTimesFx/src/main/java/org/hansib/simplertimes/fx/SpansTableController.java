/*-
 * GPL for SimplerTimes - https://github.com/hansi-b/SimplerTimes
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
import java.util.function.Function;

import javafx.beans.binding.DoubleBinding;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.scene.control.Alert.AlertType;
import javafx.scene.control.ButtonType;
import javafx.scene.control.SelectionMode;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableColumn.CellEditEvent;
import javafx.scene.control.TableView;
import javafx.scene.control.cell.ComboBoxTableCell;
import javafx.util.StringConverter;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hansib.simplertimes.fx.data.FxProject;
import org.hansib.simplertimes.fx.data.FxSpan;
import org.hansib.simplertimes.fx.data.ObservableData;
import org.hansib.simplertimes.fx.l10n.Buttons;
import org.hansib.simplertimes.fx.l10n.Headers;
import org.hansib.simplertimes.fx.l10n.MenuItems;
import org.hansib.simplertimes.times.Utils;
import org.hansib.sundries.fx.AlertBuilder;
import org.hansib.sundries.fx.ContextMenuBuilder;
import org.hansib.sundries.fx.table.CellFactoryBuilder;
import org.hansib.sundries.fx.table.EditingCell;
import org.hansib.sundries.fx.table.TableColumnBuilder;

public class SpansTableController {

	private static class ProjectComboBoxTableCell extends ComboBoxTableCell<FxSpan, FxProject> { // NOSONAR

		private final Runnable updateHandler;
		private final ObservableValue<String> name;

		ProjectComboBoxTableCell(ObservableList<FxProject> projects, Runnable updateHandler) {

			super(new StringConverter<>() {
				@Override
				public String toString(FxProject project) {
					return project == null ? null : project.text();
				}

				@Override
				public FxProject fromString(String string) {
					throw new UnsupportedOperationException();
				}
			}, projects);
			this.updateHandler = updateHandler;
			this.name = itemProperty().flatMap(FxProject::name);
			this.name.addListener((obs, oldName, newName) -> updateText());
		}

		@Override
		public void startEdit() {
			super.startEdit();
			updateText();
		}

		@Override
		public void commitEdit(FxProject newValue) {
			super.commitEdit(newValue);
			updateHandler.run();
		}

		@Override
		public void updateItem(FxProject item, boolean empty) {
			super.updateItem(item, empty);
			updateText();
			updateHandler.run();
		}

		private void updateText() {
			setText(isEditing() ? null : name.getValue());
		}
	}

	private static final Logger log = LogManager.getLogger();

	@FXML
	private TableColumn<FxSpan, FxProject> projectCol;

	@FXML
	private TableColumn<FxSpan, OffsetDateTime> startCol;

	@FXML
	private TableColumn<FxSpan, OffsetDateTime> endCol;

	@FXML
	private TableColumn<FxSpan, Duration> durationCol;

	@FXML
	private TableView<FxSpan> spansTable;

	private ObservableList<FxProject> projects;

	private Runnable updateHandler;

	@FXML
	void initialize() {
		log.info("Initialising spans table");

		DateTimeHandler dtHandler = new DateTimeHandler();

		spansTable.setEditable(true);
		spansTable.getSelectionModel().setSelectionMode(SelectionMode.MULTIPLE);

		new TableColumnBuilder<>(startCol).headerText(Headers.Start.fmt()) //
				.value(FxSpan::start) //
				.cellFactory(list -> new EditingCell<>(dtHandler.getConverter(), //
						(cell, text) -> {
							OffsetDateTime newStartTime = dtHandler.parseToOffsetDateTime(text);
							return newStartTime != null && withReferenceOffset(newStartTime, cell.getItem()).isBefore(
									endCol.getCellData(cell.getIndex()));
						})) //
				.onEditCommit(e -> setNewDateTime(e, FxSpan::start)) //
				.build();

		new TableColumnBuilder<>(endCol).headerText(Headers.End.fmt()) //
				.value(FxSpan::end) //
				.cellFactory(list -> new EditingCell<>(dtHandler.getConverter(), //
						(cell, text) -> {
							OffsetDateTime newEndTime = dtHandler.parseToOffsetDateTime(text);
							return newEndTime != null && withReferenceOffset(newEndTime, cell.getItem()).isAfter(
									startCol.getCellData(cell.getIndex()));
						})) //
				.onEditCommit(e -> setNewDateTime(e, FxSpan::end)) //
				.build();

		new TableColumnBuilder<>(projectCol).headerText(Headers.Project.fmt()) //
				.value(FxSpan::fxProject) //
				.cellFactory(list -> new ProjectComboBoxTableCell(projects, updateHandler)) //
				.editable() //
				.comparator(FxProject.nameComparator) //
				.build();

		new TableColumnBuilder<>(durationCol).headerText(Headers.Duration.fmt()) //
				.value(FxSpan::duration) //
				.cellFactory(new CellFactoryBuilder<>(durationCol).format(Utils::toHmsString).build()) //
				.build();

		startCol.prefWidthProperty().bind(spansTable.widthProperty().multiply(.25));
		endCol.prefWidthProperty().bind(spansTable.widthProperty().multiply(.25));
		projectCol.prefWidthProperty().bind(spansTable.widthProperty().multiply(.35));

		final DoubleBinding colsWidth = projectCol.widthProperty() //
				.add(startCol.widthProperty()).add(endCol.widthProperty()) //
				.multiply(1.03);

		durationCol.prefWidthProperty().bind(spansTable.widthProperty().subtract(colsWidth));

		spansTable.setContextMenu(new ContextMenuBuilder() //
				.item(MenuItems.Delete.fmt(), e -> deleteSelected()).build());
	}

	void setUpdateHandler(Runnable updateHandler) {
		this.updateHandler = updateHandler;
	}

	void setData(ObservableData data) {

		this.projects = data.projects();

		spansTable.setItems(data.spans());
		spansTable.getSortOrder().add(startCol);
		spansTable.getSortOrder().add(endCol);
		spansTable.getSortOrder().add(projectCol);
	}

	private void setNewDateTime(CellEditEvent<FxSpan, OffsetDateTime> event,
			Function<FxSpan, SimpleObjectProperty<OffsetDateTime>> propertyGetter) {
		FxSpan spanRow = event.getTableView().getItems().get(event.getTablePosition().getRow());
		propertyGetter.apply(spanRow).set(withReferenceOffset(event.getNewValue(), event.getOldValue()));
		spansTable.sort();
		updateHandler.run();
	}

	private static OffsetDateTime withReferenceOffset(OffsetDateTime target, OffsetDateTime offsetReferences) {
		return target.withOffsetSameLocal(offsetReferences.getOffset());
	}

	private void deleteSelected() {
		final ObservableList<FxSpan> selectedItems = spansTable.getSelectionModel().getSelectedItems();

		boolean userAgreed = new AlertBuilder(AlertType.WARNING,
				"The deletion of the %d selected item(s) cannot be undone.".formatted(selectedItems.size())) //
				.withDefaultButton(ButtonType.CANCEL, Buttons.Cancel.fmt()) //
				.withButton(ButtonType.YES, Buttons.Delete.fmt()) //
				.showAndWaitFor(ButtonType.YES);

		if (userAgreed) {
			spansTable.getItems().removeAll(selectedItems);
			updateHandler.run();
		}
	}
}
