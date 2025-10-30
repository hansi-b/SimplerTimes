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
package org.hansib.simplertimes.fx.tree;

import java.util.function.Supplier;

import javafx.scene.control.Button;
import javafx.stage.Stage;

import org.hansib.simplertimes.fx.ExitManager;
import org.hansib.simplertimes.fx.Icons;
import org.hansib.simplertimes.fx.data.FxProject;
import org.hansib.simplertimes.fx.data.ObservableData;
import org.hansib.simplertimes.prefs.Prefs;
import org.hansib.sundries.fx.ButtonBuilder;
import org.hansib.sundries.fx.StageData;
import org.hansib.sundries.fx.StageToggle;

public class TreeDisplay {

	public TreeDisplay(Button editTreeButton, Supplier<ObservableData> lazyData, ExitManager exitManager,
		Prefs.Windows windowPrefs) {
		StageToggle stageToggle = new StageToggle(() -> initTreeView(lazyData.get()), windowPrefs.projects);
		exitManager.addPreExitAction(() -> windowPrefs.projects = StageData.of(stageToggle.getStage()));
		new ButtonBuilder(editTreeButton).graphic(Icons.editTree()).onAction(event -> stageToggle.toggle()).build();
	}

	private static Stage initTreeView(ObservableData data) {
		TreeViewWindow<FxProject> treeViewWindow = new TreeViewWindow<>(data.fxProjectTree(), data::updateProjectList);
		treeViewWindow.setPreRemovalChecker(data.fxProjectRemovalCallback());
		return treeViewWindow.initStage();
	}
}