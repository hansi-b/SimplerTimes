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