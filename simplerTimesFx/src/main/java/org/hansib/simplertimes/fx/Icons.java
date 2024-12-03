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

import org.kordamp.ikonli.javafx.FontIcon;

import javafx.scene.Node;

public class Icons {

	public static final Node start() {
		return new FontIcon("codicon-play-circle");
	}

	public static final Node stop() {
		return new FontIcon("codicon-stop-circle");
	}

	public static final Node editTree() {
		return new FontIcon("codicon-list-tree");
	}

	public static final Node showSpans() {
		return new FontIcon("codicon-calendar");
	}

	public static final Node weekBack() {
		return new FontIcon("unil-angle-left");
	}

	public static final Node weekForward() {
		return new FontIcon("unil-angle-right");
	}

	public static final Node monthBack() {
		return new FontIcon("unil-angle-double-left");
	}

	public static final Node monthForward() {
		return new FontIcon("unil-angle-double-right");
	}

	public static final Node today() {
		return new FontIcon("unil-circle");
	}
}