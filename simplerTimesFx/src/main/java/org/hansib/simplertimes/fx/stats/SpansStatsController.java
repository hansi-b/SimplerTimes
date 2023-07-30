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
package org.hansib.simplertimes.fx.stats;

import java.time.LocalDate;
import java.time.Period;
import java.util.SortedSet;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hansib.simplertimes.fx.FxSpan;
import org.hansib.simplertimes.fx.Icons;
import org.hansib.simplertimes.times.Utils;
import org.hansib.sundries.fx.ButtonBuilder;

import javafx.beans.binding.Bindings;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;

public class SpansStatsController {

	private static final Logger log = LogManager.getLogger();

	@FXML
	private TableView<StatsRow> spansStats;

	@FXML
	Button monthBack;
	@FXML
	Button weekBack;
	@FXML
	Button today;
	@FXML
	Button weekForward;
	@FXML
	Button monthForward;

	private StatsCalculator calc;
	private ObjectProperty<LocalDate> dateShown;

	@FXML
	void initialize() {
		log.info("Initialising spans stats");

		TableColumn<StatsRow, String> projectColumn = new TableColumn<>("Project");
		projectColumn.setCellValueFactory(data -> data.getValue().project());
		spansStats.getColumns().add(projectColumn);

		dateShown = new SimpleObjectProperty<>(LocalDate.now());

		new ButtonBuilder(monthBack) //
				.graphic(Icons.monthBack()).onAction(e -> shiftDate(Period.ofMonths(-1))).build();
		new ButtonBuilder(weekBack) //
				.graphic(Icons.weekBack()).onAction(e -> shiftDate(Period.ofDays(-7))).build();

		new ButtonBuilder(today) //
				.graphic(Icons.today()).onAction(e -> setDate(LocalDate.now())).build();
		today.disableProperty()
				.bind(Bindings.createBooleanBinding(() -> LocalDate.now().equals(dateShown.get()), dateShown));

		new ButtonBuilder(weekForward) //
				.graphic(Icons.weekForward()).onAction(e -> shiftDate(Period.ofDays(7))).build();
		new ButtonBuilder(monthForward) //
				.graphic(Icons.monthForward()).onAction(e -> shiftDate(Period.ofMonths(1))).build();
	}

	public void setSpans(ObservableList<FxSpan> spans) {
		this.calc = new StatsCalculator(spans);
		updateStats();
	}

	private void setDate(LocalDate newDate) {
		dateShown.set(newDate);
		updateStats();
	}

	private void shiftDate(Period shift) {
		setDate(dateShown.get().plus(shift));
	}

	public void updateStats() {
		SortedSet<LocalDate> dates = Utils.daysOfWeek(dateShown.get());
		updateDateColumns(dates);
		fillStats(dates);
	}

	private void updateDateColumns(SortedSet<LocalDate> dates) {
		if (spansStats.getColumns().size() > 1)
			spansStats.getColumns().remove(1, spansStats.getColumns().size());

		for (LocalDate dt : dates) {
			TableColumn<StatsRow, String> odtColumn = new TableColumn<>(dt.toString());
			odtColumn.setCellValueFactory(data -> data.getValue().ldStr(dt));
			spansStats.getColumns().add(odtColumn);
		}
	}

	private void fillStats(SortedSet<LocalDate> dates) {
		ObservableList<StatsRow> items = FXCollections.observableArrayList(
				calc.allProjects().stream().map(p -> StatsRow.of(p, calc.durationsByDate(p, dates))).toList());
		spansStats.setItems(items);
	}
}
