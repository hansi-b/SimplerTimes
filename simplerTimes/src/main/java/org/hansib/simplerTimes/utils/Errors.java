package org.hansib.simplerTimes.utils;

public class Errors {

	public static IllegalArgumentException illegalArg(final String fmt, final Object... args) {
		return new IllegalArgumentException(String.format(fmt, args));
	}
}