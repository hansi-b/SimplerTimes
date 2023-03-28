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

import org.hansib.simplertimes.spans.SpansCollection;
import org.hansib.sundries.fx.ButtonDecorator;
import org.hansib.sundries.fx.FxResourceLoader;

import javafx.scene.control.Button;
import javafx.stage.Stage;

class SpansDisplay {

	private final Supplier<SpansCollection> spansSupplier;
	private Stage spansStage;
	private SpansInfoController spansInfoController;

	SpansDisplay(Button showSpansButton, Supplier<SpansCollection> spansSupplier) {

		this.spansSupplier = spansSupplier;

		new ButtonDecorator(showSpansButton).graphic(Icons.showSpans()).onAction(event -> showSpansInfo());
	}

	private void showSpansInfo() {
		if (spansStage == null) {
			spansStage = new Stage();
			spansInfoController = new FxResourceLoader().loadFxmlToStage("spansInfo.fxml", spansStage);
		}
		spansInfoController.setSpans(spansSupplier.get());
		spansStage.setTitle("Spans");

		spansStage.show();
	}
}