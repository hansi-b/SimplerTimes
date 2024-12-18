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
package org.hansib.simplertimes.fx.stats;

import java.time.LocalDate;
import java.time.Period;
import java.util.SortedSet;

import javafx.beans.InvalidationListener;
import javafx.beans.binding.Bindings;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hansib.simplertimes.fx.Icons;
import org.hansib.simplertimes.fx.data.FxSpan;
import org.hansib.simplertimes.times.Utils;
import org.hansib.sundries.fx.ButtonBuilder;
import org.hansib.sundries.fx.table.TableColumnBuilder;

public class SpansStatsController {

	private static final Logger log = LogManager.getLogger();

	@FXML
	private TableView<StatsRow> spansStats;

	@FXML
	private Button monthBack;
	@FXML
	private Button weekBack;
	@FXML
	private Button today;
	@FXML
	private Button weekForward;
	@FXML
	private Button monthForward;

	private StatsCalculator calc;

	private ObjectProperty<LocalDate> selectedDate;
	private ObservableValue<SortedSet<LocalDate>> datesShown;

	@FXML
	void initialize() {
		log.info("Initialising spans stats");

		spansStats.getColumns()
				.add(new TableColumnBuilder<StatsRow, String>("Project").value(StatsRow::project).build());

		selectedDate = new SimpleObjectProperty<>(LocalDate.now());
		datesShown = selectedDate.map(Utils::daysOfWeek);
		datesShown.addListener((InvalidationListener) observable -> updateStats());

		new ButtonBuilder(monthBack) //
				.graphic(Icons.monthBack()).onAction(e -> shiftDate(Period.ofMonths(-1))).build();
		new ButtonBuilder(weekBack) //
				.graphic(Icons.weekBack()).onAction(e -> shiftDate(Period.ofDays(-7))).build();

		new ButtonBuilder(today) //
				.graphic(Icons.today()).onAction(e -> setDate(LocalDate.now())).build();
		today.disableProperty()
				.bind(Bindings.createBooleanBinding(() -> LocalDate.now().equals(selectedDate.get()), selectedDate));

		new ButtonBuilder(weekForward) //
				.graphic(Icons.weekForward()).onAction(e -> shiftDate(Period.ofDays(7))).build();
		new ButtonBuilder(monthForward) //
				.graphic(Icons.monthForward()).onAction(e -> shiftDate(Period.ofMonths(1))).build();
	}

	public void setSpans(ObservableList<FxSpan> spans) {
		this.calc = new StatsCalculator(spans);
		spans.addListener((InvalidationListener) observable -> updateStats());
		updateStats();
	}

	private void setDate(LocalDate newDate) {
		selectedDate.set(newDate);
	}

	private void shiftDate(Period shift) {
		setDate(selectedDate.get().plus(shift));
	}

	public void updateStats() {
		updateDateColumns();
		spansStats.setItems(calc.calcItems(datesShown.getValue()));
	}

	private void updateDateColumns() {
		ObservableList<TableColumn<StatsRow, ?>> columns = spansStats.getColumns();
		if (columns.size() > 1)
			columns.remove(1, columns.size());

		for (LocalDate dt : datesShown.getValue()) {
			columns.add(new TableColumnBuilder<StatsRow, String>(dt.toString()).value(d -> d.ldStr(dt)).build());
		}
	}
}
