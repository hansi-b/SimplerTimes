package org.hansib.simplertimes.fx;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

import org.hansib.simplertimes.fx.tree.TextFieldTreeNode;
import org.hansib.simplertimes.projects.Project;

/**
 * Maintains a 1-to-1 mapping with a {@link Project}
 */
public class FxProject implements TextFieldTreeNode<FxProject> {

	public static final Comparator<FxProject> nameComparator = (FxProject o1, FxProject o2) -> Project.nameComparator
			.compare(o1.project, o2.project);

	private final Project project;

	private final FxProject parent;
	private final List<FxProject> children;

	private FxProject(FxProject parent, Project project) {
		this.project = project;
		this.parent = parent;
		this.children = new ArrayList<>();
	}

	static FxProject root(Project root) {
		return link(null, root);
	}

	private static FxProject link(FxProject fxParent, Project project) {
		FxProject fxBase = new FxProject(fxParent, project);
		project.children().forEach(c -> fxBase.children.add(link(fxBase, c)));
		return fxBase;
	}

	List<FxProject> flatList() {
		return flatList(new ArrayList<>());
	}

	private List<FxProject> flatList(List<FxProject> accu) {
		if (text() != null)
			accu.add(this);
		children.forEach(c -> c.flatList(accu));
		return accu;
	}

	public Project project() {
		return project;
	}

	@Override
	public String text() {
		return project.name();
	}

	public String fullName() {
		/*
		 * other options: · • › » ▹ ▷ | – #
		 */
		return String.join(" › ", project.nameWords());
	}

	@Override
	public Iterable<FxProject> children() {
		return children;
	}

	@Override
	public void setText(String newText) {
		project.setName(newText);
	}

	@Override
	public void remove() {
		project.parent().remove(project);
		parent.children.remove(this);
	}

	@Override
	public FxProject addChild(String childText) {
		FxProject child = new FxProject(this, project.add(childText));
		children.add(child);
		return child;
	}
}