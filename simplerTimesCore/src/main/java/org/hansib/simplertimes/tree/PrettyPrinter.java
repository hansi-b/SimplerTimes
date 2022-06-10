package org.hansib.simplertimes.tree;

class PrettyPrinter {

	private static final int DEFAULT_INDENTATION = 3;

	private final TreeNode node;
	private final int indentation;
	private final int currDepth;

	PrettyPrinter(TreeNode node) {
		this(node, DEFAULT_INDENTATION, 0);
	}

	PrettyPrinter(TreeNode node, int indentation) {
		this(node, indentation, 0);
	}

	private PrettyPrinter(TreeNode node, int indentation, int depth) {
		this.node = node;
		this.indentation = indentation;
		this.currDepth = depth;
	}

	String toPrettyString() {
		StringBuilder resultBuilder = new StringBuilder(" ".repeat(currDepth)).append(nameStr()).append('\n');
		for (TreeNode c : node.children())
			resultBuilder.append(new PrettyPrinter(c, indentation, currDepth + indentation).toPrettyString());

		return resultBuilder.toString();
	}

	private String nameStr() {
		Project e = node.project();
		return e == null ? "null" : e.name();
	}
}