package org.hansib.simpler_times;

import org.hansib.simpler_times.times.Span;

record LabelledSpan(String label, Span span) {
	static LabelledSpan of(String label, Span span) {
		return new LabelledSpan(label, span);
	}
}

interface LabelledSpans {
	void add(LabelledSpan span);
}