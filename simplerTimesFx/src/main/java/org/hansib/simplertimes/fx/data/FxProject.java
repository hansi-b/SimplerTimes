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
package org.hansib.simplertimes.fx.data;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;

import javafx.beans.property.ReadOnlyStringProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;

import org.hansib.simplertimes.fx.tree.TreeItemNode;
import org.hansib.simplertimes.projects.Project;
import org.hansib.sundries.Strings;

/**
 * Maintains a 1-to-1 mapping with a {@link Project}
 */
public class FxProject implements TreeItemNode<FxProject> {

	public static final Comparator<FxProject> nameComparator = (FxProject o1, FxProject o2) -> Project.nameComparator
			.compare(o1.project, o2.project);

	private final Map<Project, FxProject> fxByProject;

	private final Project project;
	private final StringProperty name;

	private FxProject(Map<Project, FxProject> fxByProject, Project project) {
		this.fxByProject = fxByProject;
		this.project = project;

		this.name = new SimpleStringProperty(project.name());
		this.name.addListener((observable, oldValue, newValue) -> project.setName(newValue));

		fxByProject.put(project, this);
	}

	static FxProject root(Project project) {
		return link(new HashMap<>(), project);
	}

	private static FxProject link(Map<Project, FxProject> fxByProject, Project project) {
		FxProject fxProject = new FxProject(fxByProject, project);
		project.children().forEach(c -> link(fxByProject, c));
		return fxProject;
	}

	List<FxProject> flatList() {
		return flatList(new ArrayList<>());
	}

	private List<FxProject> flatList(List<FxProject> accu) {
		if (text() != null)
			accu.add(this);
		children().forEach(c -> c.flatList(accu));
		return accu;
	}

	Project project() {
		return project;
	}

	@Override
	public String text() {
		return name.get();
	}

	public ReadOnlyStringProperty name() {
		return name;
	}

	public String fullName() {
		/*
		 * other options: · • › » ▹ ▷ | – #
		 */
		return String.join(" › ", project.namesList());
	}

	public String formatName(FxProject relativeTo, String delimiter) {
		return String.join(delimiter, project.namesList(relativeTo.project));
	}
	
	public boolean hasChildren() {
		return project.hasChildren();
	}

	@Override
	public Stream<FxProject> children() {
		return project.children().stream().map(fxByProject::get);
	}

	/**
	 * @return all children that do not have children (if this project has no
	 *         children, then an empty stream)
	 */
	public Stream<FxProject> leafChildren() {
		return project.dfStream().filter(c -> c != project && c.children().isEmpty()).map(fxByProject::get);
	}

	@Override
	public void sortChildren(Comparator<String> comparator) {
		project.sortChildren((o1, o2) -> comparator.compare(o1.name(), o2.name()));
	}

	@Override
	public void setText(String newText) {
		name.set(newText);
	}

	@Override
	public void remove() {
		project.parent().remove(project);
		fxByProject.remove(project);
	}

	@Override
	public FxProject addChild(String childText) {
		return new FxProject(fxByProject, project.add(childText));
	}

	@Override
	public boolean canMoveTo(FxProject newParent, int newIndex) {
		return project.canMoveTo(newParent.project, newIndex);
	}

	@Override
	public void moveTo(FxProject newParent, int newIndex) {
		project.moveTo(newParent.project, newIndex);
	}

	@Override
	public String toString() {
		return Strings.idStr(this, name.get());
	}
}