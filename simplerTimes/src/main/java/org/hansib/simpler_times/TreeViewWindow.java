package org.hansib.simpler_times;

import javafx.geometry.Bounds;
import javafx.scene.Node;
import javafx.scene.Scene;
import javafx.scene.control.TextField;
import javafx.scene.layout.StackPane;
import javafx.stage.Stage;

class TreeViewWindow {
	static void openTreeViewWindow(Node parent) {
		TextField txtField = new TextField();

		StackPane treeLayout = new StackPane();
		treeLayout.getChildren().add(txtField);

		Scene secondScene = new Scene(treeLayout, 230, 100);

		Stage window = new Stage();
		// window.initStyle(StageStyle.UNDECORATED);
		window.setScene(secondScene);

		Bounds boundsInScene = parent.localToScreen(parent.getBoundsInLocal());
		window.setX(boundsInScene.getMinX());
		window.setY(boundsInScene.getMinY());

		window.show();
	}
}