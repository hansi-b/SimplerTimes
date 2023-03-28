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

import org.kordamp.ikonli.javafx.FontIcon;

import javafx.scene.Node;

class Icons {

	static final Node start() {
		return new FontIcon("codicon-play-circle");
	}

	static final Node stop() {
		return new FontIcon("codicon-stop-circle");
	}

	static final Node editTree() {
		return new FontIcon("codicon-list-tree");
	}

	static final Node showSpans() {
		return new FontIcon("codicon-calendar");
	}
}