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