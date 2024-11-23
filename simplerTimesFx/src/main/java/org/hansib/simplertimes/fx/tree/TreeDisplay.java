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
package org.hansib.simplertimes.fx.tree;

import java.util.function.Supplier;

import org.hansib.simplertimes.fx.Icons;
import org.hansib.simplertimes.fx.data.FxProject;
import org.hansib.simplertimes.fx.data.ObservableData;
import org.hansib.sundries.fx.ButtonBuilder;
import org.hansib.sundries.fx.StageToggle;

import javafx.scene.control.Button;

public class TreeDisplay {

	private StageToggle stageToggle;

	public TreeDisplay(Button editTreeButton, Supplier<ObservableData> lazyData) {
		new ButtonBuilder(editTreeButton).graphic(Icons.editTree()).onAction(event -> showWindow(lazyData)).build();
	}

	private void showWindow(Supplier<ObservableData> lazyData) {
		if (stageToggle == null) {
			ObservableData data = lazyData.get();
			TreeViewWindow<FxProject> treeViewWindow = new TreeViewWindow<>(data.fxProjectTree(),
					data::updateProjectList);
			treeViewWindow.setPreRemovalChecker(data.fxProjectRemovalCallback());
			stageToggle = new StageToggle(treeViewWindow::initStage);
		}
		stageToggle.toggle();
	}
}