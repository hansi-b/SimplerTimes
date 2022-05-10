package org.hansib.simpler_times.spans;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import net.harawata.appdirs.AppDirs;
import net.harawata.appdirs.AppDirsFactory;

public class SpansStore {

	private static final Logger log = LogManager.getLogger();

	private final Path spansPath;

	public SpansStore() {
		this.spansPath = spansPath();
	}

	public SpansCollection load() {
		if (!spansPath.toFile().isFile())
			return new SpansCollection();
		try {
			return SpansCollection.fromYaml(Files.readString(spansPath));
		} catch (IOException e) {
			log.error(String.format("Encountered exception while trying to read spans from '%s'", spansPath), e);
		}
		return new SpansCollection();
	}

	public void save(SpansCollection spans) throws IOException {
		Files.writeString(spansPath(), spans.toYaml());
	}

	private static Path spansPath() {
		AppDirs appDirs = AppDirsFactory.getInstance();
		Path path = Path.of(appDirs.getUserDataDir("SimplerTimes", "", "HansiB"));
		if (!path.toFile().exists())
			path.toFile().mkdirs();
		return path.resolve("spans.yml");
	}
}
