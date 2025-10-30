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

import java.util.function.Supplier;

import javafx.scene.control.Button;
import javafx.stage.Stage;

import org.hansib.simplertimes.fx.data.ObservableData;
import org.hansib.simplertimes.fx.l10n.General;
import org.hansib.simplertimes.prefs.Prefs;
import org.hansib.sundries.fx.ButtonBuilder;
import org.hansib.sundries.fx.ControllerLoader;
import org.hansib.sundries.fx.StageData;
import org.hansib.sundries.fx.StageToggle;

class SpansDisplay {

	private final Supplier<ObservableData> lazyData;

	SpansDisplay(Button showSpansButton, Supplier<ObservableData> lazyData, ExitManager exitManager,
		Prefs.Windows windowPrefs) {

		this.lazyData = lazyData;

		StageToggle stageToggle = new StageToggle(() -> initStage(windowPrefs, exitManager), windowPrefs.spans);
		new ButtonBuilder(showSpansButton) //
			.graphic(Icons.showSpans()).onAction(event -> stageToggle.toggle()) //
			.build();
	}

	private Stage initStage(Prefs.Windows windowPrefs, ExitManager exitManager) {
		Stage spansStage = new Stage();
		spansStage.setTitle(General.SpansWindowTitle.fmt());

		SpansInfoController spansInfoController = ControllerLoader.<SpansInfoController>of("spansInfo.fxml")
			.withTargetStage(spansStage).load();
		spansInfoController.setData(lazyData.get());

		new Resources().loadLogo(logo -> spansStage.getIcons().add(logo));
		exitManager.addPreExitAction(() -> windowPrefs.spans = StageData.of(spansStage));
		return spansStage;
	}
}