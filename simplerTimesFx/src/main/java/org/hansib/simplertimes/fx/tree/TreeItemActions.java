/*-
 * GPL for SimplerTimes - https://github.com/hansi-b/SimplerTimes
 *
 * Copyright (C) 2022-2026 Hans Bering
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

import java.util.Comparator;
import java.util.function.Predicate;

import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeView;

import org.hansib.simplertimes.fx.l10n.MenuItems;

record TreeItemActions<T extends TreeItemNode<T>>(TreeItem<T> item) {

  void newTreeItem(TreeView<T> treeview) {
    T newChild = item.getValue().addChild(MenuItems.NewProject.fmt());
    TreeItem<T> newItem = new TreeItem<>(newChild);
    item.getChildren().add(newItem);
    treeview.getSelectionModel().select(newItem);
    treeview.edit(newItem);
  }

  void removeItem(Predicate<T> acceptRemoval, Runnable itemsChangeHandler) {
    T node = item.getValue();

    boolean removalAccepted = acceptRemoval == null || acceptRemoval.test(node);
    if (!removalAccepted) return;

    node.remove();
    item.getParent().getChildren().remove(item);
    itemsChangeHandler.run();
  }

  void sortChildren(Runnable itemsChangeHandler) {
    item.getChildren().sort(Comparator.comparing(o -> o.getValue().text()));
    item.getValue().sortChildren(String::compareTo);
    itemsChangeHandler.run();
  }

  void expandChildren() {
    item.getChildren().forEach(TreeItemActions::expand);
  }

  private static <T extends TreeItemNode<T>> void expand(TreeItem<T> item) {
    item.setExpanded(true);
    item.getChildren().forEach(TreeItemActions::expand);
  }

  void collapseChildren() {
    item.getChildren().forEach(TreeItemActions::collapse);
  }

  private static <T extends TreeItemNode<T>> void collapse(TreeItem<T> item) {
    item.getChildren().forEach(TreeItemActions::collapse);
    item.setExpanded(false);
  }
}
