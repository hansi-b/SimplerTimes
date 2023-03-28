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
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.SortedSet;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hansib.simplertimes.projects.Project;
import org.hansib.simplertimes.spans.SpansCollection;
import org.hansib.simplertimes.times.Utils;

import javafx.beans.property.ObjectProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
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

	private SpansCollection spansCollection;

	@FXML
	void initialize() {
		log.info("Initialising spans stats");

	}

	public void setSpans(SpansCollection spansCollection) {
		this.spansCollection = spansCollection;

		StatsCalculator calc = new StatsCalculator(spansCollection);

		SortedSet<LocalDate> allDates = calc.allDates();
		Set<Project> allProjects = calc.allProjects();

		TableColumn<Stats, String> projectColumn = new TableColumn<>("Project");
		projectColumn.setCellValueFactory(data -> data.getValue().project);
		spansStats.getColumns().add(projectColumn);

		for (LocalDate dt : allDates) {
			TableColumn<Stats, String> odtColumn = new TableColumn<>(dt.toString());
			odtColumn.setCellValueFactory(data -> data.getValue().ldStr(dt));
			spansStats.getColumns().add(odtColumn);
		}

		ObservableList<Stats> items = FXCollections.observableArrayList();
		items.addAll(allProjects.stream().map(p -> Stats.of(p, calc.get(p, allDates))).toList());
		spansStats.setItems(items);

	}
}
