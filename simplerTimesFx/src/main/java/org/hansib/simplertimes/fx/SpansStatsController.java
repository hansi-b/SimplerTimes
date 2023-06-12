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
import java.time.LocalDate;
import java.time.Period;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.SortedSet;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hansib.simplertimes.projects.Project;
import org.hansib.simplertimes.spans.SpansCollection;
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

	private static record Stats(ObjectProperty<String> project, Map<LocalDate, ObjectProperty<String>> durations) {

		static Stats of(Project p, Map<LocalDate, Duration> durations) {
			Map<LocalDate, ObjectProperty<String>> res = new HashMap<>();
			durations.forEach((odt, d) -> res.put(odt, new SimpleObjectProperty<>(Utils.toHmsString(d))));
			return new Stats(new SimpleObjectProperty<>(p.name()), res);
		}

		ObjectProperty<String> ldStr(LocalDate odt) {
			return durations.get(odt);
		}
	}

	@FXML
	private TableView<Stats> spansStats;

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

	private SpansCollection spansCollection;

	private ObjectProperty<LocalDate> dateProp;

	@FXML
	void initialize() {
		log.info("Initialising spans stats");

		TableColumn<Stats, String> projectColumn = new TableColumn<>("Project");
		projectColumn.setCellValueFactory(data -> data.getValue().project);
		spansStats.getColumns().add(projectColumn);

		dateProp = new SimpleObjectProperty<>(LocalDate.now());

		new ButtonBuilder(monthBack) //
				.graphic(Icons.monthBack()).onAction(e -> shiftDate(Period.ofMonths(-1))).build();
		new ButtonBuilder(weekBack) //
				.graphic(Icons.weekBack()).onAction(e -> shiftDate(Period.ofDays(-7))).build();

		new ButtonBuilder(today) //
				.graphic(Icons.today()).onAction(e -> setDate(LocalDate.now())).build();
		today.disableProperty()
				.bind(Bindings.createBooleanBinding(() -> LocalDate.now().equals(dateProp.get()), dateProp));

		new ButtonBuilder(weekForward) //
				.graphic(Icons.weekForward()).onAction(e -> shiftDate(Period.ofDays(7))).build();
		new ButtonBuilder(monthForward) //
				.graphic(Icons.monthForward()).onAction(e -> shiftDate(Period.ofMonths(1))).build();
	}

	public void setSpans(SpansCollection spansCollection) {
		this.spansCollection = spansCollection;

		updateStats();
	}

	private void setDate(LocalDate newDate) {
		dateProp.set(newDate);
		updateStats();
	}

	private void shiftDate(Period shift) {
		dateProp.set(dateProp.get().plus(shift));
		updateStats();
	}

	private void updateStats() {
		SortedSet<LocalDate> allDates = Utils.daysOfWeek(dateProp.get());
		updateDateColumns(allDates);

		fillStats(allDates);
	}

	private void updateDateColumns(SortedSet<LocalDate> allDates) {
		if (spansStats.getColumns().size() > 1)
			spansStats.getColumns().remove(1, spansStats.getColumns().size());

		for (LocalDate dt : allDates) {
			TableColumn<Stats, String> odtColumn = new TableColumn<>(dt.toString());
			odtColumn.setCellValueFactory(data -> data.getValue().ldStr(dt));
			spansStats.getColumns().add(odtColumn);
		}
	}

	private void fillStats(SortedSet<LocalDate> allDates) {
		StatsCalculator calc = new StatsCalculator(spansCollection);
		Set<Project> allProjects = calc.allProjects();
		ObservableList<Stats> items = FXCollections.observableArrayList();
		items.addAll(allProjects.stream().map(p -> Stats.of(p, calc.get(p, allDates))).toList());
		spansStats.setItems(items);
	}
}
