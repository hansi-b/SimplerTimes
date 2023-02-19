package org.hansib.simplertimes.projects;

class PrettyPrinter {

	private static final int DEFAULT_INDENTATION = 3;

	private final Project node;
	private final int indentation;
	private final int currDepth;

	PrettyPrinter(Project node) {
		this(node, DEFAULT_INDENTATION, 0);
	}

	PrettyPrinter(Project node, int indentation) {
		this(node, indentation, 0);
	}

	private PrettyPrinter(Project node, int indentation, int depth) {
		this.node = node;
		this.indentation = indentation;
		this.currDepth = depth;
	}

	String toPrettyString() {
		StringBuilder resultBuilder = new StringBuilder(" ".repeat(currDepth)).append(String.valueOf(node.name()))
				.append('\n');
		for (Project c : node.children())
			resultBuilder.append(new PrettyPrinter(c, indentation, currDepth + indentation).toPrettyString());

		return resultBuilder.toString();
	}
}