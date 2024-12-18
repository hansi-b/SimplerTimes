/*-
 * GPL for SimplerTimes - https://github.com/hansi-b/SimplerTimes
 *
 * Copyright (C) 2022-2024 Hans Bering
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

import javafx.scene.control.ComboBox;

import org.hansib.simplertimes.fx.data.FxProject;
import org.hansib.sundries.fx.Converters;

class FxConverters {

	static void setComboBoxProjectConverter(ComboBox<FxProject> projectSelection) {
		projectSelection.setConverter( //
				new Converters().stringConverter( //
						proj -> proj == null ? "" : proj.fullName(), //
						projName -> projName == null || projName.isBlank() ? null
								: projectSelection.getSelectionModel().getSelectedItem()));
	}
}
