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
import org.hansib.simplertimes.fx.l10n.MenuItems;
import org.hansib.sundries.fx.ContextMenuBuilder;
import org.hansib.sundries.fx.FxResourceLoader;

import javafx.geometry.Bounds;
import javafx.scene.Node;
import javafx.scene.Scene;
import javafx.scene.control.ContextMenu;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeView;
import javafx.scene.image.Image;
import javafx.scene.layout.StackPane;
import javafx.stage.Stage;
import javafx.stage.WindowEvent;

public class TreeViewWindow<T extends TextFieldTreeNode<T>> {

	private static final Logger log = LogManager.getLogger();

	private final TreeView<T> treeView;

	private final Runnable updateHandler;

	private final ContextMenu contextMenu;

	private Stage stage;

	TreeViewWindow(Supplier<T> rootSupplier, Runnable updateHandler) {
		TreeItem<T> rootItem = linkToTreeItem(rootSupplier.get());
		rootItem.setExpanded(true);
		this.treeView = initTreeView(rootItem);

		this.contextMenu = new ContextMenuBuilder() //
				.item(MenuItems.NewProject.fmt(), t -> addItem(treeView, treeView.getRoot())) //
				.build();

		this.updateHandler = updateHandler;
	}

	private static <T extends TextFieldTreeNode<T>> TreeItem<T> linkToTreeItem(T treeNode) {
		TreeItem<T> treeItem = new TreeItem<>(treeNode);
		treeNode.children().forEach(c -> treeItem.getChildren().add(linkToTreeItem(c)));
		return treeItem;
	}

	private static <T extends TextFieldTreeNode<T>> TreeView<T> initTreeView(TreeItem<T> rootItem) {
		TreeView<T> tree = new TreeView<>(rootItem);
		tree.setEditable(true);
		tree.setShowRoot(false);
		tree.setCellFactory(p -> new TextFieldTreeCellImpl<>(tree));
		return tree;
	}

	void show(Node parent) {
		if (stage == null)
			stage = initStage(parent, initTreeScene());

		if (stage.isShowing())
			stage.hide();
		else
			stage.show();
	}

	private Stage initStage(Node parent, Scene secondScene) {
		Stage stage = new Stage();
		stage.setTitle("Projects");

		Image logo = new FxResourceLoader().loadImage("logo.png");
		if (logo == null)
			log.warn("Could not load application icon");
		else
			stage.getIcons().add(logo);

		stage.setScene(secondScene);

		Bounds boundsInScene = parent.localToScreen(parent.getBoundsInLocal());
		stage.setX(boundsInScene.getMinX());
		stage.setY(boundsInScene.getMinY());

		stage.addEventHandler(WindowEvent.WINDOW_CLOSE_REQUEST, e -> updateHandler.run());
		return stage;
	}

	private Scene initTreeScene() {
		StackPane treePane = new StackPane();
		treePane.getChildren().add(treeView);
		treePane.setOnContextMenuRequested(
				e -> contextMenu.show(treePane.getScene().getWindow(), e.getScreenX(), e.getScreenY()));

		return new Scene(treePane, 250, 250);
	}

	static <T extends TextFieldTreeNode<T>> void addItem(TreeView<T> treeview, TreeItem<T> parent) {
		TreeItem<T> current = parent != null ? parent : treeview.getRoot();
		T nodeChild = current.getValue().addChild(MenuItems.NewProject.fmt());
		TreeItem<T> newItem = new TreeItem<>(nodeChild);
		current.getChildren().add(newItem);
		treeview.getSelectionModel().select(newItem);
		treeview.edit(newItem);
	}
}