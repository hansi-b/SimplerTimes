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

import java.util.ArrayList;
import java.util.List;

import javafx.beans.Observable;
import javafx.beans.binding.Bindings;
import javafx.beans.binding.BooleanBinding;
import javafx.beans.property.ReadOnlyObjectProperty;
import javafx.scene.control.TreeItem;

record TreeItemBindings<T extends TreeItemNode<T>>(TreeItem<T> item) {

  BooleanBinding isLastProject() {
    ReadOnlyObjectProperty<TreeItem<T>> parentProp = item.parentProperty();

    return Bindings.isNull(parentProp.get().parentProperty())
        .and(Bindings.equal(Bindings.size(parentProp.get().getChildren()), 1));
  }

  BooleanBinding hasFewerThan2Children() {
    return Bindings.lessThan(Bindings.size(item.getChildren()), 2);
  }

  BooleanBinding areAllChildrenExpanded() {
    return Bindings.createBooleanBinding(
        () -> item.getChildren().stream().allMatch(this::isExpanded), treeDependencies());
  }

  BooleanBinding areAllChildrenCollapsed() {
    return Bindings.createBooleanBinding(
        () -> item.getChildren().stream().allMatch(this::isCollapsed), treeDependencies());
  }

  private Observable[] treeDependencies() {
    List<Observable> dependencies = new ArrayList<>();
    collectTreeDependencies(item, dependencies);
    return dependencies.toArray(new Observable[0]);
  }

  private void collectTreeDependencies(TreeItem<T> item, List<Observable> dependencies) {
    dependencies.add(item.expandedProperty());
    dependencies.add(item.getChildren());
    item.getChildren().forEach(c -> collectTreeDependencies(c, dependencies));
  }

  private boolean isExpanded(TreeItem<T> item) {
    if (item == null) return false;
    return item.getChildren().isEmpty()
        || (item.isExpanded() && item.getChildren().stream().allMatch(this::isExpanded));
  }

  private boolean isCollapsed(TreeItem<T> item) {
    if (item == null) return false;
    return item.getChildren().isEmpty()
        || (!item.isExpanded() && item.getChildren().stream().allMatch(this::isCollapsed));
  }
}
