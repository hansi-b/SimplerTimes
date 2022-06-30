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
	 * 
	 */
	public static class Builder {
		private static record ProjectStub(String name, LinkedHashSet<Long> children) {
			private ProjectStub(String name) {
				this(name, new LinkedHashSet<>());
			}
		}

		private final long rootId;
		private final Map<Long, ProjectStub> projectById;

		private boolean locked;

		public Builder(long rootId, String rootName) {
			this.rootId = rootId;
			this.projectById = new HashMap<>();

			this.locked = false;

			projectById.put(rootId, new ProjectStub(rootName));
		}

		/**
		 * FIXME: despite both parent and child being the same type, this only works
		 * bottom-up - there is no use in adding anything to the childBuilder later on
		 * TODO: it would not be difficult to update the childBuilder with the merge to
		 * make this symmetric - is that useful?
		 */
		public Builder mergeChild(Builder childBuilder) {

			if (locked)
				throw Errors.illegalState("Cannot merge child to locked parent builder");
			if (childBuilder.locked)
				throw Errors.illegalState("Cannot merge child from locked child builder");

			childBuilder.projectById.forEach((id, stub) -> {
				projectById.merge(id, stub, (oldVal, newVal) -> {
					throw Errors.illegalArg("Cannot add duplicate #%d (old: '%s', new: '%s')", id, oldVal, newVal);
				});
			});
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

	private final AtomicLong nextId;

	private final long id;

	private String name;

	private Project parent;

	private final List<Project> children;

	private Project(AtomicLong nextId, long id, String name, Project parent, List<Project> children) {
		this.nextId = nextId;
		this.id = id;
		this.name = name;
		this.parent = parent;
		this.children = children;
	}

	private Project(AtomicLong nextId, String name, Project parent, List<Project> children) {
		this(nextId, nextId.getAndIncrement(), name, parent, children);
	}

	public static Project root() {
		return new Project(new AtomicLong(), null, null, new ArrayList<>());
	}

	public Project add(String name) {
		Project child = new Project(nextId, name, this, new ArrayList<>());
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

	public String name() {
		return name;
	}

	public void setName(String newName) {
		name = newName;
	}

	public String fullProjectName() {
		LinkedList<String> hierarchy = new LinkedList<>();
		for (Project current = this; current.parent != null; current = current.parent) {
			if (current.name != null)
				hierarchy.addFirst(current.name);
		}
		return String.join(" ▸ ", hierarchy);
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

	public Stream<Project> dfStream() {
		Stream<Project> c = children.stream().flatMap(Project::dfStream);
		return Stream.concat(Stream.of(this), c);
	}

	Stream<Project> filter(Set<String> words) {
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
