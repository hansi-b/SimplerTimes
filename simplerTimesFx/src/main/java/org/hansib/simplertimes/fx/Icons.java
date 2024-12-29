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

import javafx.scene.Node;

import org.kordamp.ikonli.javafx.FontIcon;

public class Icons {

	public static Node start() {
		return new FontIcon("codicon-play-circle");
	}

	public static Node stop() {
		return new FontIcon("codicon-stop-circle");
	}

	public static Node editTree() {
		return new FontIcon("codicon-list-tree");
	}

	public static Node showSpans() {
		return new FontIcon("codicon-calendar");
	}

	public static Node weekBack() {
		return new FontIcon("unil-angle-left");
	}

	public static Node weekForward() {
		return new FontIcon("unil-angle-right");
	}

	public static Node monthBack() {
		return new FontIcon("unil-angle-double-left");
	}

	public static Node monthForward() {
		return new FontIcon("unil-angle-double-right");
	}

	public static Node today() {
		return new FontIcon("unil-circle");
	}
}