package org.hansib.simpler_times;

import java.util.ArrayList;
import java.util.List;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

class SpansCollection implements Spans {

	private static final Logger log = LogManager.getLogger();

	private final List<Span> spans;

	SpansCollection() {
		spans = new ArrayList<>();
	}

	@Override
	public void add(Span span) {
		log.info(span);
		spans.add(span);
	}
}