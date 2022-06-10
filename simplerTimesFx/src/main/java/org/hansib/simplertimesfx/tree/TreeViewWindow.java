package org.hansib.simplertimesfx.tree;

import java.util.function.Supplier;

import org.hansib.simplertimes.tree.Project;
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

	private final TreeItem<TreeNode> rootItem;

	public TreeViewWindow(TreeNode root) {
		rootItem = copyToTreeItem(root);
	}

	public void openTreeViewWindow(Node parent) {

		rootItem.setExpanded(true);

		TreeView<TreeNode> tree = new TreeView<>(rootItem);
		tree.setEditable(true);
		tree.setShowRoot(false);
		tree.setCellFactory(p -> new TextFieldTreeCellImpl(tree, () -> new Project("New Subproject")));

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

	private void showMenu(ContextMenuEvent e, Window owner, TreeView<TreeNode> tree,
			Supplier<Project> newProjectSupplier) {
		ContextMenu contextMenu = new ContextMenu();

		contextMenu.getItems().add(getAddMenuItem(tree, newProjectSupplier));
		contextMenu.show(owner, e.getScreenX(), e.getScreenY());
	}

	private MenuItem getAddMenuItem(TreeView<TreeNode> treeview, Supplier<Project> newProjectSupplier) {
		MenuItem addMenuItem = new MenuItem("New project");
		addMenuItem.setOnAction(t -> {
			TreeItem<TreeNode> current = treeview.getRoot();
			TreeNode nodeChild = current.getValue().add(newProjectSupplier.get());
			TreeItem<TreeNode> newItem = new TreeItem<>(nodeChild);
			current.getChildren().add(newItem);
			treeview.getSelectionModel().select(newItem);
			treeview.edit(newItem);
		});
		return addMenuItem;
	}

	private static TreeItem<TreeNode> copyToTreeItem(TreeNode treeNode) {
		TreeItem<TreeNode> treeItem = new TreeItem<>(treeNode);
		treeNode.children().forEach(c -> {
			TreeItem<TreeNode> i = copyToTreeItem(c);
			treeItem.getChildren().add(i);
		});
		return treeItem;
	}
}