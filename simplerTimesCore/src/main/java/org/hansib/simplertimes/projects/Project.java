package org.hansib.simplertimes.projects;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashSet;
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
	 * A bottom-up builder for a project tree: You have to add children by 'merging'
	 * builders for those children, and this operation is not symmetric: Only the
	 * parent builder can be used on afterwards.
	 */
	public static class BottomUpBuilder {
		private static record ProjectStub(String name, LinkedHashSet<Long> children) {
			private ProjectStub(String name) {
				this(name, new LinkedHashSet<>());
			}
		}

		private final long rootId;
		private final Map<Long, ProjectStub> projectById;

		private boolean locked;

		public BottomUpBuilder(long rootId, String rootName) {
			this.rootId = rootId;
			this.projectById = new HashMap<>();

			this.locked = false;

			projectById.put(rootId, new ProjectStub(rootName));
		}

		/**
		 * Merge the projects in the argument builder as children into this builder.
		 * 
		 * NB: Merging is not symmetrical, but has to happen bottom up. To ensure this,
		 * the argument childBuilder is locked after merging.
		 */
		public BottomUpBuilder mergeChild(BottomUpBuilder childBuilder) {

			if (locked)
				throw Errors.illegalState("Cannot merge child to locked parent builder");
			if (childBuilder.locked)
				throw Errors.illegalState("Cannot merge child from locked child builder");

			childBuilder.projectById.forEach((childId, stub) -> projectById.merge(childId, stub, (oldVal, newVal) -> {
				throw Errors.illegalArg("Cannot add duplicate #%d (old: '%s', new: '%s')", childId, oldVal, newVal);
			}));
			projectById.get(rootId).children.add(childBuilder.rootId);
			childBuilder.locked = true;

			return this;
		}

		public Project build() {
			return build(new AtomicLong(Collections.max(projectById.keySet()) + 1L), rootId, null);
		}

		private Project build(AtomicLong nextId, long id, Project parent) {
			ProjectStub stub = projectById.get(id);
			ArrayList<Project> children = new ArrayList<>();

			Project project = new Project(nextId, id, stub.name, parent, children);
			children.addAll(stub.children.stream().map(childId -> build(nextId, childId, project)).toList());
			return project;
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

		int i = children.indexOf(child);
		if (i < 0)
			throw new IllegalArgumentException(
					String.format("Unknown child %s for %s (have %s)", child, this, children));
		Project removed = children.remove(i);
		removed.parent = null;
		return removed;
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
		LinkedList<String> hierarchy = new LinkedList<>();
		for (Project current = this; current.parent != null; current = current.parent) {
			if (current.name != null)
				hierarchy.addFirst(current.name);
		}
		return hierarchy;
	}

	public void setProject(String newName) {
		this.name = newName;
	}

	public Project parent() {
		return parent;
	}

	public List<Project> children() {
		return Collections.unmodifiableList(children);
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
		return Strings.idStr(this, String.format("%s(%d)", name, id));
	}
}
