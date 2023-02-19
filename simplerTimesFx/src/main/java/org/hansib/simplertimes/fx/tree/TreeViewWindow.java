/**
 * Abakus - https://github.com/hansi-b/SimplerTimesFx
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
package org.hansib.simplertimes.fx.tree;

import java.util.function.Supplier;

import org.hansib.simplertimes.fx.l10n.MenuItems;
import org.hansib.simplertimes.projects.Project;

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
import javafx.stage.WindowEvent;

public class TreeViewWindow {

	private Runnable closeHandler;

	private final TreeItem<Project> rootItem;

	public TreeViewWindow(Project root) {
		rootItem = linkToTreeItem(root);
	}

	private static TreeItem<Project> linkToTreeItem(Project treeNode) {
		TreeItem<Project> treeItem = new TreeItem<>(treeNode);
		treeNode.children().forEach(c -> {
			TreeItem<Project> i = linkToTreeItem(c);
			treeItem.getChildren().add(i);
		});
		return treeItem;
	}

	public void openTreeViewWindow(Node parent) {

		rootItem.setExpanded(true);

		TreeView<Project> tree = new TreeView<>(rootItem);
		tree.setEditable(true);
		tree.setShowRoot(false);
		tree.setCellFactory(p -> new TextFieldTreeCellImpl(tree, MenuItems.NewProject::fmt));

		StackPane treeLayout = new StackPane();
		treeLayout.getChildren().add(tree);

		treeLayout.setOnContextMenuRequested(
				e -> showMenu(e, treeLayout.getScene().getWindow(), tree, MenuItems.NewProject::fmt));

		Scene secondScene = new Scene(treeLayout, 230, 100);

		Stage window = new Stage();
		// window.initStyle(StageStyle.UNDECORATED);
		window.setScene(secondScene);

		Bounds boundsInScene = parent.localToScreen(parent.getBoundsInLocal());
		window.setX(boundsInScene.getMinX());
		window.setY(boundsInScene.getMinY());

		if (closeHandler != null)
			window.addEventHandler(WindowEvent.WINDOW_CLOSE_REQUEST, e -> closeHandler.run());
		window.show();
	}

	public TreeViewWindow withCloseHandler(Runnable closeHandler) {
		this.closeHandler = closeHandler;
		return this;
	}

	private void showMenu(ContextMenuEvent e, Window owner, TreeView<Project> tree,
			Supplier<String> newProjectSupplier) {
		ContextMenu contextMenu = new ContextMenu();

		contextMenu.getItems().add(getAddMenuItem(tree, newProjectSupplier));
		contextMenu.show(owner, e.getScreenX(), e.getScreenY());
	}

	private MenuItem getAddMenuItem(TreeView<Project> treeview, Supplier<String> newProjectSupplier) {
		MenuItem addMenuItem = new MenuItem("New project");
		addMenuItem.setOnAction(t -> {
			TreeItem<Project> current = treeview.getRoot();
			Project nodeChild = current.getValue().add(newProjectSupplier.get());
			TreeItem<Project> newItem = new TreeItem<>(nodeChild);
			current.getChildren().add(newItem);
			treeview.getSelectionModel().select(newItem);
			treeview.edit(newItem);
		});
		return addMenuItem;
	}
}