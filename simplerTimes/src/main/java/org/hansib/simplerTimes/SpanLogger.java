package org.hansib.simplerTimes;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

class SpanLogger implements LabelledSpans {

	private static final Logger log = LogManager.getLogger();

	@Override
	public void add(LabelledSpan span) {
		log.info(span);
	}
}