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
package org.hansib.simplertimes.fx.data;

import java.util.List;

import org.hansib.simplertimes.fx.tree.TreeItemNode.PreRemovalCallback;
import org.hansib.sundries.fx.AlertBuilder;

import javafx.collections.ObservableList;
import javafx.scene.control.Alert.AlertType;
import javafx.scene.control.ButtonType;

class FxProjectRemovalCallback implements PreRemovalCallback<FxProject> {
	private ObservableList<FxSpan> spans;

	FxProjectRemovalCallback(ObservableList<FxSpan> spans) {
		this.spans = spans;
	}

	@Override
	public boolean removalAccepted(FxProject project) {

		List<FxProject> projects = project.flatList();

		List<FxSpan> affectedSpans = spans.stream().filter(s -> projects.contains(s.fxProject().get())).toList();

		String projectDeletionWarning = "%d selected project(s)".formatted(projects.size());
		String spanDeletionWarning = affectedSpans.isEmpty() ? ""
				: " and %d associated span(s)".formatted(spans.size());

		String warning = "The deletion of %s%s cannot be undone.".formatted(projectDeletionWarning,
				spanDeletionWarning);

		boolean userAgreed = new AlertBuilder(AlertType.WARNING, warning) //
				.withDefaultButton(ButtonType.CANCEL, "Cancel") //
				.withButton(ButtonType.YES, "Delete") //
				.showAndWaitFor(ButtonType.YES);
		if (userAgreed)
			spans.removeAll(affectedSpans);
		return userAgreed;
	}
}