package org.hansib.simplertimes.projects;

class PrettyPrinter {

	private static final int DEFAULT_INDENTATION = 3;

	private final ProjectTree node;
	private final int indentation;
	private final int currDepth;

	PrettyPrinter(ProjectTree node) {
		this(node, DEFAULT_INDENTATION, 0);
	}

	PrettyPrinter(ProjectTree node, int indentation) {
		this(node, indentation, 0);
	}

	private PrettyPrinter(ProjectTree node, int indentation, int depth) {
		this.node = node;
		this.indentation = indentation;
		this.currDepth = depth;
	}

	String toPrettyString() {
		StringBuilder resultBuilder = new StringBuilder(" ".repeat(currDepth)).append(String.valueOf(node.name()))
				.append('\n');
		for (ProjectTree c : node.children())
			resultBuilder.append(new PrettyPrinter(c, indentation, currDepth + indentation).toPrettyString());

		return resultBuilder.toString();
	}
}