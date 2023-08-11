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
package org.hansib.simplertimes.fx.tree;

import java.util.function.Supplier;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hansib.simplertimes.fx.ObservableData;
import org.hansib.simplertimes.fx.l10n.MenuItems;
import org.hansib.simplertimes.projects.Project;
import org.hansib.sundries.fx.ContextMenuBuilder;
import org.hansib.sundries.fx.FxResourceLoader;

import javafx.geometry.Bounds;
import javafx.scene.Node;
import javafx.scene.Scene;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeView;
import javafx.scene.image.Image;
import javafx.scene.input.ContextMenuEvent;
import javafx.scene.layout.StackPane;
import javafx.stage.Stage;
import javafx.stage.Window;
import javafx.stage.WindowEvent;

public class TreeViewWindow {

	private static final Logger log = LogManager.getLogger();

	private final TreeItem<Project> rootItem;
	private final Runnable updateHandler;

	public TreeViewWindow(ObservableData data) {
		this.rootItem = linkToTreeItem(data.projectTree());
		this.updateHandler = data::updateProjectList;
	}

	private static TreeItem<Project> linkToTreeItem(Project treeNode) {
		TreeItem<Project> treeItem = new TreeItem<>(treeNode);
		treeNode.children().forEach(c -> {
			TreeItem<Project> i = linkToTreeItem(c);
			treeItem.getChildren().add(i);
		});
		return treeItem;
	}

	public void show(Node parent) {

		rootItem.setExpanded(true);

		TreeView<Project> tree = new TreeView<>(rootItem);
		tree.setEditable(true);
		tree.setShowRoot(false);
		tree.setCellFactory(p -> new TextFieldTreeCellImpl(tree, MenuItems.NewProject::fmt));

		StackPane treeLayout = new StackPane();
		treeLayout.getChildren().add(tree);

		treeLayout.setOnContextMenuRequested(e -> showMenu(e, treeLayout.getScene().getWindow(), tree));

		Scene secondScene = new Scene(treeLayout, 250, 250);

		Stage window = new Stage();
		window.setTitle("Projects");

		Image logo = new FxResourceLoader().loadImage("logo.png");
		if (logo == null)
			log.warn("Could not load application icon");
		else
			window.getIcons().add(logo);

		window.setScene(secondScene);

		Bounds boundsInScene = parent.localToScreen(parent.getBoundsInLocal());
		window.setX(boundsInScene.getMinX());
		window.setY(boundsInScene.getMinY());

		window.addEventHandler(WindowEvent.WINDOW_CLOSE_REQUEST, e -> updateHandler.run());
		window.show();
	}

	private void showMenu(ContextMenuEvent e, Window owner, TreeView<Project> tree) {
		new ContextMenuBuilder() //
				.item("New project", t -> createNewProject(tree, MenuItems.NewProject::fmt)) //
				.build().show(owner, e.getScreenX(), e.getScreenY());
	}

	private static void createNewProject(TreeView<Project> treeview, Supplier<String> newProjectSupplier) {
		TreeItem<Project> current = treeview.getRoot();
		Project nodeChild = current.getValue().add(newProjectSupplier.get());
		TreeItem<Project> newItem = new TreeItem<>(nodeChild);
		current.getChildren().add(newItem);
		treeview.getSelectionModel().select(newItem);
		treeview.edit(newItem);
	}
}