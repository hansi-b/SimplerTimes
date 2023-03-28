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
package org.hansib.simplertimes.spans;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Objects;
import java.util.stream.Stream;

public class SpansCollection implements Iterable<Span> {

	public final List<Span> spans;

	private SpansCollection(List<Span> spans) {
		this.spans = spans;
	}

	public SpansCollection() {
		this(new ArrayList<>());
	}

	static SpansCollection with(Span... spans) {
		return new SpansCollection(new ArrayList<>(Arrays.asList(spans)));
	}

	public void add(Span span) {
		spans.add(span);
	}

	public List<Span> view() {
		return Collections.unmodifiableList(spans);
	}

	public Stream<Span> stream() {
		return spans.stream();
	}

	@Override
	public Iterator<Span> iterator() {
		return spans.iterator();
	}

	@Override
	public boolean equals(Object obj) {
		if (!(obj instanceof SpansCollection other))
			return false;
		return spans.equals(other.spans);
	}

	@Override
	public int hashCode() {
		return Objects.hash(spans.hashCode());
	}

	@Override
	public String toString() {
		return String.format("Spans:%s", spans);
	}
}