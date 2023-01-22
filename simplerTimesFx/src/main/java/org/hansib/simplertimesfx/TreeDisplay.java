package org.hansib.simplertimesfx;

import java.util.function.Supplier;

import org.hansib.simplertimes.projects.Project;
import org.hansib.simplertimesfx.tree.TreeViewWindow;

import javafx.scene.control.Button;

class TreeDisplay {

	TreeDisplay(Button editTreeButton, Supplier<Project> rootSupplier, Runnable closeHandler) {

		editTreeButton.setGraphic(Icons.editTree());
		editTreeButton.setOnAction(event -> new TreeViewWindow(rootSupplier.get()).withCloseHandler(closeHandler)
				.openTreeViewWindow(editTreeButton));
	}
}