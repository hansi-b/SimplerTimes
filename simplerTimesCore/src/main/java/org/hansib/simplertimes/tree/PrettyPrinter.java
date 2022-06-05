package org.hansib.simplertimes.tree;

import java.util.function.Function;

class PrettyPrinter<E extends Nameable> {

	private static final int DEFAULT_INDENTATION = 3;

	private final TreeNode<E> node;
	private Function<E, String> toString;
	private final int indentation;
	private final int currDepth;

	PrettyPrinter(TreeNode<E> node) {
		this(node, e -> e == null ? "<null>" : e.name());
	}

	PrettyPrinter(TreeNode<E> node, Function<E, String> element2String) {
		this(node, element2String, DEFAULT_INDENTATION, 0);
	}

	PrettyPrinter(TreeNode<E> node, Function<E, String> element2String, int indentation) {
		this(node, element2String, indentation, 0);
	}

	private PrettyPrinter(TreeNode<E> node, Function<E, String> element2String, int indentation, int depth) {
		this.node = node;
		this.toString = element2String;
		this.indentation = indentation;
		this.currDepth = depth;
	}

	String toPrettyString() {
		StringBuilder resultBuilder = new StringBuilder(" ".repeat(currDepth)).append(toString.apply(node.element()))
				.append('\n');
		for (TreeNode<E> c : node.children())
			resultBuilder
					.append(new PrettyPrinter<>(c, toString, indentation, currDepth + indentation).toPrettyString());

		return resultBuilder.toString();
	}
}