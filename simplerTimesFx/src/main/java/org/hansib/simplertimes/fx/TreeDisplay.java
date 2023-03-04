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

import java.util.function.Supplier;

import org.hansib.simplertimes.fx.tree.TreeViewWindow;
import org.hansib.simplertimes.projects.Project;

import javafx.scene.control.Button;

class TreeDisplay {

	TreeDisplay(Button editTreeButton, Supplier<Project> rootSupplier, Runnable closeHandler) {

		editTreeButton.setGraphic(Icons.editTree());
		editTreeButton.setOnAction(event -> new TreeViewWindow(rootSupplier.get()).withCloseHandler(closeHandler)
				.openTreeViewWindow(editTreeButton));
	}
}