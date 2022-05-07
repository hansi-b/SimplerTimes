package org.hansib.simplerTimes;

import org.hansib.simplerTimes.times.Span;

record LabelledSpan(String label, Span span) {
	static LabelledSpan of(String label, Span span) {
		return new LabelledSpan(label, span);
	}
}

interface LabelledSpans {
	void add(LabelledSpan span);
}