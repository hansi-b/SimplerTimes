package org.hansib.simplertimes.yaml;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hansib.simplertimes.AppData;
import org.hansib.simplertimes.spans.SpansCollection;

public class SpansStore {

	private static final Logger log = LogManager.getLogger();

	private final Path spansPath;

	public SpansStore() {
		this.spansPath = new AppData().dataPath("spans.yml");
	}

	public SpansCollection load() {
		if (!spansPath.toFile().isFile())
			return new SpansCollection();
		try {
			return SpansYamlConverter.fromYaml(Files.readString(spansPath));
		} catch (IOException e) {
			log.error(String.format("Encountered exception while trying to read spans from '%s'", spansPath), e);
			return new SpansCollection();
		}
	}

	public void save(SpansCollection spans) throws IOException {
		Files.writeString(spansPath, SpansYamlConverter.toYaml(spans));
	}
}
