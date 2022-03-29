package org.hansib.simplerTimes.times;

import java.time.LocalDateTime;

public interface TimesRepo {
	void addSpan(LocalDateTime start, LocalDateTime end);
}