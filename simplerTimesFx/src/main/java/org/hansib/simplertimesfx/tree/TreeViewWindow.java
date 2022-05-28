package org.hansib.simplertimesfx.tree;

import org.hansib.simplertimes.Project;
import org.hansib.simplertimes.tree.TreeNode;

import javafx.geometry.Bounds;
import javafx.scene.Node;
import javafx.scene.Scene;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeView;
import javafx.scene.layout.StackPane;
import javafx.stage.Stage;

public class TreeViewWindow {

	public static void openTreeViewWindow(Node parent) {

		TreeItem<TreeNode<Project>> rootItem = new TreeItem<>(TreeNode.root());
		TreeView<TreeNode<Project>> tree = new TreeView<>(rootItem);
		tree.setEditable(true);
		// tree.setShowRoot(false);
		tree.setCellFactory(p -> new TextFieldTreeCellImpl<Project>(tree, () -> new Project("New Project")));
		rootItem.setExpanded(true);

		StackPane treeLayout = new StackPane();
		treeLayout.getChildren().add(tree);

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