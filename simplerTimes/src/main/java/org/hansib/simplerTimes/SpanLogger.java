package org.hansib.simplerTimes;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hansib.simplerTimes.times.Span;

class SpanLogger implements SpanCollection {

	private static final Logger log = LogManager.getLogger();

	@Override
	public void add(Span span) {
		log.info(span);
	}
}