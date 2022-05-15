package org.hansib.simpler_times;

import org.hansib.simpler_times.fx.SimplerTimesFx;

/*
 * Needed as a workaround for https://github.com/javafxports/openjdk-jfx/issues/236
 */
public class AppStarter {

	public static void main(String[] args) {
		SimplerTimesFx.main(args);
	}
}
