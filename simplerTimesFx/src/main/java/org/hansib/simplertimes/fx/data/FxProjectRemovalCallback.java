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