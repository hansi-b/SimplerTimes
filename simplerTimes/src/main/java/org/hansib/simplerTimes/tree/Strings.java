package org.hansib.simplerTimes.tree;

import java.util.stream.IntStream;

public class Strings {

	private Strings() {
		// instantiation prevention
	}

	/**
	 * @param id some semantic identifier of the object
	 * @return a canonical representation in the format
	 *         [className]@[objectHexHash](@id)
	 */
	public static <T> String idStr(T o, String id) {
		return String.format("%s@%08X(%s)", o.getClass().getSimpleName(), ((Object) o).hashCode(), id);
	}

	public static String join(String joiner, String[] arr, int beginIncl, int endExcl) {
		return String.join(joiner, IntStream.range(beginIncl, endExcl).boxed().map(i -> arr[i]).toList());
	}
}
