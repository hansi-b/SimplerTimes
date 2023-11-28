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

import java.util.function.Supplier;

import org.hansib.simplertimes.fx.data.ObservableData;
import org.hansib.sundries.fx.ButtonBuilder;
import org.hansib.sundries.fx.FxResourceLoader;
import org.hansib.sundries.fx.StageToggle;

import javafx.scene.control.Button;
import javafx.stage.Stage;

class SpansDisplay {

	private final Supplier<ObservableData> lazyData;

	SpansDisplay(Button showSpansButton, Supplier<ObservableData> lazyData) {

		this.lazyData = lazyData;

		StageToggle stageToggle = new StageToggle(this::initStage);
		new ButtonBuilder(showSpansButton) //
				.graphic(Icons.showSpans()).onAction(event -> stageToggle.toggle()) //
				.build();
	}

	private Stage initStage() {
		Stage spansStage = new Stage();
		spansStage.setTitle("Spans");

		FxResourceLoader fxLoader = new FxResourceLoader();
		SpansInfoController spansInfoController = fxLoader.loadFxmlToStage("spansInfo.fxml", spansStage);
		spansInfoController.setData(lazyData.get());

		new Resources().loadLogo(logo -> spansStage.getIcons().add(logo));
		return spansStage;
	}
}