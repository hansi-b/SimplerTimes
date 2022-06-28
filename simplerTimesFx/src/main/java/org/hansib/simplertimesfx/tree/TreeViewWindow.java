package org.hansib.simplertimesfx.tree;

import java.util.function.Supplier;

import org.hansib.simplertimes.projects.ProjectTree;

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

	private final TreeItem<ProjectTree> rootItem;

	public TreeViewWindow(ProjectTree root) {
		rootItem = copyToTreeItem(root);
	}

	public void openTreeViewWindow(Node parent) {

		rootItem.setExpanded(true);

		TreeView<ProjectTree> tree = new TreeView<>(rootItem);
		tree.setEditable(true);
		tree.setShowRoot(false);
		tree.setCellFactory(p -> new TextFieldTreeCellImpl(tree, () -> "New Subproject"));

		StackPane treeLayout = new StackPane();
		treeLayout.getChildren().add(tree);

		treeLayout.setOnContextMenuRequested(
				e -> showMenu(e, treeLayout.getScene().getWindow(), tree, () -> "New Project"));

		Scene secondScene = new Scene(treeLayout, 230, 100);

		Stage window = new Stage();
		// window.initStyle(StageStyle.UNDECORATED);
		window.setScene(secondScene);

		Bounds boundsInScene = parent.localToScreen(parent.getBoundsInLocal());
		window.setX(boundsInScene.getMinX());
		window.setY(boundsInScene.getMinY());

		window.show();
	}

	private void showMenu(ContextMenuEvent e, Window owner, TreeView<ProjectTree> tree,
			Supplier<String> newProjectSupplier) {
		ContextMenu contextMenu = new ContextMenu();

		contextMenu.getItems().add(getAddMenuItem(tree, newProjectSupplier));
		contextMenu.show(owner, e.getScreenX(), e.getScreenY());
	}

	private MenuItem getAddMenuItem(TreeView<ProjectTree> treeview, Supplier<String> newProjectSupplier) {
		MenuItem addMenuItem = new MenuItem("New project");
		addMenuItem.setOnAction(t -> {
			TreeItem<ProjectTree> current = treeview.getRoot();
			ProjectTree nodeChild = current.getValue().add(newProjectSupplier.get());
			TreeItem<ProjectTree> newItem = new TreeItem<>(nodeChild);
			current.getChildren().add(newItem);
			treeview.getSelectionModel().select(newItem);
			treeview.edit(newItem);
		});
		return addMenuItem;
	}

	private static TreeItem<ProjectTree> copyToTreeItem(ProjectTree treeNode) {
		TreeItem<ProjectTree> treeItem = new TreeItem<>(treeNode);
		treeNode.children().forEach(c -> {
			TreeItem<ProjectTree> i = copyToTreeItem(c);
			treeItem.getChildren().add(i);
		});
		return treeItem;
	}
}