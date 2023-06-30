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

import java.util.ArrayList;
import java.util.stream.Collectors;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hansib.simplertimes.spans.SpansCollection;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.scene.control.Tab;

public class SpansInfoController {

	private static final Logger log = LogManager.getLogger();

	@FXML
	private Tab spansTableTab;

	@FXML
	private SpansTableController spansTableController;

	@FXML
	private Tab spansStatsTab;

	@FXML
	private SpansStatsController spansStatsController;

	@FXML
	void initialize() {
		log.info("Initialising spans info");
	}

	public void setSpans(SpansCollection spansCollection) {
		ObservableList<SpanRow> observableSpans = FXCollections.observableArrayList(
				spansCollection.stream().map(SpanRow::new).collect(Collectors.toCollection(() -> new ArrayList<>())));
		spansTableController.setSpans(observableSpans);
		spansStatsController.setSpans(spansCollection);
	}
}
