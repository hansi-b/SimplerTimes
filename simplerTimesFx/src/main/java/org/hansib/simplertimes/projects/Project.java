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
package org.hansib.simplertimes.projects;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicLong;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.hansib.sundries.Errors;
import org.hansib.sundries.Strings;

public class Project {

	/**
	 * Compares projects by name.
	 */
	public static final Comparator<Project> nameComparator = (p1, p2) -> {
		if (p1.name() == null)
			return -1;
		if (p2.name() == null)
			return 1;
		return p1.name().compareTo(p2.name());
	};

	/**
	 * A builder for a project tree. Basically a mutable mirror of Project to allow
	 * for predefined ids. Building checks id consistency.
	 */
	public static class Builder {

		private final long id;
		private final String name;
		private final List<Builder> children;

		public Builder(long id, String name) {
			this.id = id;
			this.name = name;
			this.children = new ArrayList<>();
		}

		public void addChild(Builder builder) {
			children.add(builder);
		}

		/**
		 * @return a project tree containing the node of this builder and all its
		 *         children
		 */
		public Project build() {

			AtomicLong idGenerator = new AtomicLong();
			Project root = new Project(idGenerator, id, name, null, new ArrayList<>());

			Map<Long, Project> knownProjectsById = new HashMap<>();
			knownProjectsById.put(id, root);

			children.forEach(cBuilder -> cBuilder.buildRecursively(idGenerator, root, knownProjectsById));

			idGenerator.set(1 + Collections.max(knownProjectsById.keySet()));
			return root;
		}

		private Project buildRecursively(AtomicLong idGenerator, Project parent, Map<Long, Project> knownProjectsById) {

			if (knownProjectsById.containsKey(id))
				throw Errors.illegalArg("Duplicate id %d: New name '%s', old '%s'", id, name,
						knownProjectsById.get(id).name);

			Project current = new Project(idGenerator, id, name, parent, new ArrayList<>());
			parent.children.add(current);
			knownProjectsById.put(id, current);

			children.forEach(cBuilder -> cBuilder.buildRecursively(idGenerator, current, knownProjectsById));
			return current;
		}
	}

	private final AtomicLong idGenerator;

	private final long id;

	private String name;

	private Project parent;

	private final List<Project> children;

	private Project(AtomicLong idGenerator, long id, String name, Project parent, List<Project> children) {
		this.idGenerator = idGenerator;
		this.id = id;
		this.name = name;
		this.parent = parent;
		this.children = children;
	}

	private Project(AtomicLong idGenerator, String name, Project parent, List<Project> children) {
		this(idGenerator, idGenerator.getAndIncrement(), name, parent, children);
	}

	public static Project root() {
		return new Project(new AtomicLong(), null, null, new ArrayList<>());
	}

	public Project add(String name) {
		Project child = new Project(idGenerator, name, this, new ArrayList<>());
		children.add(child);
		return child;
	}

	public Project remove(Project child) {

		boolean removed = children.remove(child);
		if (!removed)
			throw Errors.illegalArg("Unknown child %s for %s (have %s)", child, this, children);
		child.parent = null;
		return child;
	}

	/**
	 * Checks the preconditions for moving this project to the argument parent
	 * project at the argument child list index.
	 * <ol>
	 * <li>Both projects must be in the same project tree (have the same id
	 * generator).
	 * <li>The new parent must not be the same as or a descendant of this project.
	 * <li>If the new parent is the current parent, the argument index must not be
	 * this project's index in the children list.
	 * </ol>
	 * 
	 * @param newParent the project which would become the new parent
	 * @param newIndex  the new index in the new parent's children list
	 * @return true if this project can moved to become a child of the argument node
	 */
	public boolean canMoveTo(Project newParent, int newIndex) {
		if (idGenerator != newParent.idGenerator)
			return false;
		if (newParent == this.parent)
			return newIndex != parent.children.indexOf(this);
		return dfStream().noneMatch(c -> c == newParent);
	}

	/**
	 * Makes this project to the argument parent project at the argument child list
	 * index.
	 * 
	 * @param newParent the new parent for this project
	 * @param index     the target index in the new parent's child list; if this
	 *                  item is already in that list, then the index is interpreted
	 *                  as the index in the list with the current item first removed
	 *                  from the list
	 * @throws IllegalArgumentException if the preconditions defined by
	 *                                  {@link #canMoveTo(Project, int)} are not
	 *                                  fullfilled
	 * 
	 */
	public void moveTo(Project newParent, int newIndex) {
		if (!canMoveTo(newParent, newIndex))
			throw Errors.illegalArg("Cannot make %s a child of %s", this, newParent);
		parent.children.remove(this);
		parent = newParent;
		parent.children.add(newIndex, this);
	}

	public long id() {
		return id;
	}

	/**
	 * Returns the project with the argument id from this project tree, including
	 * this project itself. Only looks "downwards" into children (not upwards at
	 * parents).
	 * 
	 * @param id the project id to search for
	 * @return the project beneath this project (including this) with the argument
	 *         id; null if there is no such project
	 */
	public Project findById(long id) {
		return dfStream().filter(p -> p.id == id).findFirst().orElse(null);
	}

	public String name() {
		return name;
	}

	public void setName(String newName) {
		name = newName;
	}

	/**
	 * @return the ordered list of non-null names, beginning with the root down to
	 *         this project
	 */
	public List<String> nameWords() {
		LinkedList<String> words = new LinkedList<>();
		for (Project current = this; current.parent != null; current = current.parent) {
			if (current.name != null)
				words.addFirst(current.name);
		}
		return words;
	}

	public Project parent() {
		return parent;
	}

	public List<Project> children() {
		return Collections.unmodifiableList(children);
	}

	public void sortChildren(Comparator<Project> comparator) {
		children.sort(comparator);
	}

	/**
	 * @return a depth-first stream of this project and its descendants.
	 */
	public Stream<Project> dfStream() {
		Stream<Project> c = children.stream().flatMap(Project::dfStream);
		return Stream.concat(Stream.of(this), c);
	}

	public Stream<Project> filter(Set<String> words) {
		if (words.isEmpty())
			return dfStream();
		Set<String> wordsStillMissing = name == null ? words
				: words.stream().filter(w -> !name.contains(w)).collect(Collectors.toSet());
		Stream<Project> filteredChildren = children.stream().flatMap(c -> c.filter(wordsStillMissing));
		return wordsStillMissing.isEmpty() ? Stream.concat(Stream.of(this), filteredChildren) : filteredChildren;
	}

	@Override
	public String toString() {
		return Strings.idStr(this, "%s(%d)".formatted(name, id));
	}
}
