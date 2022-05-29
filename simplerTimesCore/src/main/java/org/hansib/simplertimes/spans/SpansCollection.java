package org.hansib.simplertimes.spans;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

public class SpansCollection {

	public final List<Span> spans;

	public SpansCollection() {
		spans = new ArrayList<>();
	}

	@JsonCreator
	public SpansCollection(@JsonProperty("spans") List<Span> spans) {
		this.spans = spans;
	}

	static SpansCollection with(Span... spans) {
		SpansCollection sc = new SpansCollection();
		for (Span s : spans)
			sc.add(s);
		return sc;
	}

	public void add(Span span) {
		spans.add(span);
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