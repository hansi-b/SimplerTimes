package org.hansib.simplertimes;

import java.nio.file.Path;

import net.harawata.appdirs.AppDirsFactory;

public class AppData {
	private final Path userDataDir;

	public AppData() {
		userDataDir = Path.of(AppDirsFactory.getInstance().getUserDataDir("SimplerTimes", "", "HansiB"));
		if (!userDataDir.toFile().exists())
			userDataDir.toFile().mkdirs();
	}

	public Path dataPath(String filename) {
		return userDataDir.resolve(filename);
	}
}