/*-
 * GPL for SimplerTimes - https://github.com/hansi-b/SimplerTimes
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

import javafx.scene.Scene;
import javafx.scene.control.ContextMenu;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeView;
import javafx.scene.layout.StackPane;
import javafx.stage.Stage;

import org.hansib.simplertimes.fx.Resources;
import org.hansib.simplertimes.fx.l10n.General;
import org.hansib.simplertimes.fx.l10n.MenuItems;
import org.hansib.simplertimes.fx.tree.TreeItemNode.PreRemovalCallback;
import org.hansib.sundries.ResourceLoader;
import org.hansib.sundries.fx.ContextMenuBuilder;

public class TreeViewWindow<T extends TreeItemNode<T>> {

	public static final String PROJECT_PANE_FX_ID = "ProjectTreePane";

	private final TreeView<T> treeView;
	private final Runnable itemsChangeHandler;

	private PreRemovalCallback<T> removalChecker;

	public TreeViewWindow(T root, Runnable itemsChangeHandler) {
		this.treeView = initTreeView(TreeItemNode.linkTree(root), itemsChangeHandler);
		this.treeView.getStylesheets().add(new ResourceLoader().getResourceUrl("fxml/app.css").toString());
		this.itemsChangeHandler = itemsChangeHandler;
	}

	private TreeView<T> initTreeView(TreeItem<T> rootItem, Runnable changeHandler) {
		rootItem.setExpanded(true);

		TreeView<T> tree = new TreeView<>(rootItem);
		tree.setEditable(true);
		tree.setShowRoot(false);

		TreeCellDragAndDrop<T> dnd = new TreeCellDragAndDrop<>(changeHandler);
		tree.setCellFactory(p -> dnd.withDragAndDrop(new TextFieldTreeCellImpl<T>(changeHandler) //
				.withContextMenu(this::createContextMenu)));
		return tree;
	}

	private ContextMenu createContextMenu(TextFieldTreeCellImpl<T> cell) {
		TreeItem<T> treeItem = cell.getTreeItem();
		if (treeItem.getParent() == null)
			return null;

		TreeItemBindings<T> bindings = new TreeItemBindings<>(treeItem);
		TreeItemActions<T> actions = new TreeItemActions<>(treeItem);
		return new ContextMenuBuilder() //
				.item(MenuItems.NewSubproject.fmt(), e -> actions.newTreeItem(cell.getTreeView())) //
				.item(MenuItems.Delete.fmt(),
						e -> actions.removeItem(n -> removalChecker.removalAccepted(n), itemsChangeHandler),
						bindings.isLastProject()) //
				.item(MenuItems.SortChildren.fmt(), e -> actions.sortChildren(itemsChangeHandler),
						bindings.hasFewerThan2Children()) //
				.build();
	}

	public void setPreRemovalChecker(PreRemovalCallback<T> preRemovalCallback) {
		this.removalChecker = preRemovalCallback;
	}

	public Stage initStage() {
		Stage treeStage = new Stage();
		treeStage.setTitle(General.ProjectsWindowTitle.fmt());

		new Resources().loadLogo(logo -> treeStage.getIcons().add(logo));

		treeStage.setScene(initTreePaneScene());
		return treeStage;
	}

	private Scene initTreePaneScene() {

		StackPane treePane = new StackPane();
		treePane.setId(PROJECT_PANE_FX_ID);

		TreeItem<T> root = treeView.getRoot();
		TreeItemBindings<T> bindings = new TreeItemBindings<>(root);
		TreeItemActions<T> actions = new TreeItemActions<>(root);
		ContextMenu contextMenu = new ContextMenuBuilder() //
				.item(MenuItems.NewProject.fmt(), t -> actions.newTreeItem(treeView)) //
				.separator() //
				.item(MenuItems.ExpandAll.fmt(), t -> actions.expandChildren(), bindings.areAllChildrenExpanded()) //
				.item(MenuItems.CollapseAll.fmt(), t -> actions.collapseChildren(), bindings.areAllChildrenCollapsed()) //
				.build();
		treePane.setOnContextMenuRequested(
				e -> contextMenu.show(treePane.getScene().getWindow(), e.getScreenX(), e.getScreenY()));

		treePane.getChildren().add(treeView);
		return new Scene(treePane, 250, 250);
	}

}