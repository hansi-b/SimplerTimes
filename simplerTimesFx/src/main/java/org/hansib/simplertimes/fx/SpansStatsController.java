/**
 * SimplerTimes - https://github.com/hansi-b/SimplerTimesFx
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

import java.time.OffsetDateTime;
import java.util.Set;
import java.util.SortedSet;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hansib.simplertimes.projects.Project;
import org.hansib.simplertimes.spans.SpansCollection;

import javafx.beans.property.ObjectProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;

public class SpansStatsController {

	private static final Logger log = LogManager.getLogger();

	private static record Stats(ObjectProperty<String> project) {
		Stats(Project p) {
			this(new SimpleObjectProperty<>(p.name()));
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

		SortedSet<OffsetDateTime> allDates = calc.allDates();
		Set<Project> allProjects = calc.allProjects();

		TableColumn<Stats, String> projectColumn = new TableColumn<>("Project");
		projectColumn.setCellValueFactory(data -> data.getValue().project);
		spansStats.getColumns().add(projectColumn);

		ObservableList<Stats> items = FXCollections.observableArrayList();
		items.addAll(allProjects.stream().map(Stats::new).toList());
		spansStats.setItems(items);
	}
}
