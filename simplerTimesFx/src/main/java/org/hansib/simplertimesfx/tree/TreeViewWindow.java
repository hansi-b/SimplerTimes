package org.hansib.simplertimesfx.tree;

import java.util.function.Supplier;

import org.hansib.simplertimes.Project;
import org.hansib.simplertimes.tree.TreeNode;

import javafx.geometry.Bounds;
import javafx.scene.Node;
import javafx.scene.Scene;
import javafx.scene.control.ContextMenu;
import javafx.scene.control.MenuItem;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeView;
import javafx.scene.input.ContextMenuEvent;
import javafx.scene.layout.StackPane;
import javafx.stage.Stage;
import javafx.stage.Window;

public class TreeViewWindow {

	private final TreeItem<TreeNode<Project>> rootItem;

	public TreeViewWindow(TreeNode<Project> root) {
		rootItem = copyToTreeItem(root);
	}

	public void openTreeViewWindow(Node parent) {

		rootItem.setExpanded(true);

		TreeView<TreeNode<Project>> tree = new TreeView<>(rootItem);
		tree.setEditable(true);
		tree.setShowRoot(false);
		tree.setCellFactory(p -> new TextFieldTreeCellImpl<Project>(tree, () -> new Project("New Subproject")));

		StackPane treeLayout = new StackPane();
		treeLayout.getChildren().add(tree);

		treeLayout.setOnContextMenuRequested(
				e -> showMenu(e, treeLayout.getScene().getWindow(), tree, () -> new Project("New Project")));

		Scene secondScene = new Scene(treeLayout, 230, 100);

		Stage window = new Stage();
		// window.initStyle(StageStyle.UNDECORATED);
		window.setScene(secondScene);

		Bounds boundsInScene = parent.localToScreen(parent.getBoundsInLocal());
		window.setX(boundsInScene.getMinX());
		window.setY(boundsInScene.getMinY());

		window.show();
	}

	private void showMenu(ContextMenuEvent e, Window owner, TreeView<TreeNode<Project>> tree,
			Supplier<Project> newProjectSupplier) {
		ContextMenu contextMenu = new ContextMenu();

		contextMenu.getItems().add(getAddMenuItem(tree, newProjectSupplier));
		contextMenu.show(owner, e.getScreenX(), e.getScreenY());
	}

	private MenuItem getAddMenuItem(TreeView<TreeNode<Project>> treeview, Supplier<Project> newProjectSupplier) {
		MenuItem addMenuItem = new MenuItem("New project");
		addMenuItem.setOnAction(t -> {
			TreeItem<TreeNode<Project>> current = treeview.getRoot();
			TreeNode<Project> nodeChild = current.getValue().add(newProjectSupplier.get());
			TreeItem<TreeNode<Project>> newItem = new TreeItem<>(nodeChild);
			current.getChildren().add(newItem);
			treeview.getSelectionModel().select(newItem);
			treeview.edit(newItem);
		});
		return addMenuItem;
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