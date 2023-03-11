package org.hansib.simplertimes.times;

import java.time.Duration;

public class Utils {

	public static String toHmsString(Duration duration) {
		return String.format("%d:%02d:%02d", duration.toHours(), duration.toMinutesPart(), duration.toSecondsPart());
	}

}
