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

	public static void openTreeViewWindow(Node parent, TreeNode<Project> root) {

		TreeItem<TreeNode<Project>> rootItem = copyToTreeItem(root);
		rootItem.setExpanded(true);

		TreeView<TreeNode<Project>> tree = new TreeView<>(rootItem);
		tree.setEditable(true);
		// tree.setShowRoot(false);
		tree.setCellFactory(p -> new TextFieldTreeCellImpl<Project>(tree, () -> new Project("New Project")));

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

	private static TreeItem<TreeNode<Project>> copyToTreeItem(TreeNode<Project> treeNode) {
		TreeItem<TreeNode<Project>> treeItem = new TreeItem<>(treeNode);
		treeNode.children().forEach(c -> {
			TreeItem<TreeNode<Project>> i = copyToTreeItem(c);
			treeItem.getChildren().add(i);
		});
		return treeItem;
	}
}