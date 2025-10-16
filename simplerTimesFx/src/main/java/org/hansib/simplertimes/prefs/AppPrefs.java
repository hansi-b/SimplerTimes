package org.hansib.simplertimes.prefs;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hansib.simplertimes.DataPaths;

public class AppPrefs {

	public static AppPrefs instance;

	private static final Logger log = LogManager.getLogger();

	public Prefs.AppVersion version = Prefs.AppVersion.v1;
	public Prefs.Disclaimer disclaimer = new Prefs.Disclaimer();
	public Prefs.Windows windows = new Prefs.Windows();

	public static AppPrefs get() {
		if (instance == null)
			instance = load();
		return instance;
	}

	private static AppPrefs load() {
		Path prefsPath = DataPaths.atDefault().prefsPath();
		if (!prefsPath.toFile().isFile())
			return new AppPrefs();
		try {
			String yaml = Files.readString(prefsPath);
			ObjectMapper mapper = new ObjectMapper(new YAMLFactory());
			mapper.setVisibility(mapper.getSerializationConfig().getDefaultVisibilityChecker()
				.withFieldVisibility(JsonAutoDetect.Visibility.ANY).withGetterVisibility(JsonAutoDetect.Visibility.NONE)
				.withIsGetterVisibility(JsonAutoDetect.Visibility.NONE)
				.withSetterVisibility(JsonAutoDetect.Visibility.NONE)
				.withCreatorVisibility(JsonAutoDetect.Visibility.NONE));
			return mapper.readValue(yaml, AppPrefs.class);
		} catch (IOException e) {
			log.error(String.format("Encountered exception while trying to read preferences from '%s'", prefsPath), e);
			return new AppPrefs();
		}
	}

	public void save() {
		Path prefsPath = DataPaths.atDefault().prefsPath();
		ObjectMapper mapper = new ObjectMapper(new YAMLFactory());
		mapper.setVisibility(mapper.getSerializationConfig().getDefaultVisibilityChecker()
			.withFieldVisibility(JsonAutoDetect.Visibility.ANY).withGetterVisibility(JsonAutoDetect.Visibility.NONE)
			.withIsGetterVisibility(JsonAutoDetect.Visibility.NONE).withSetterVisibility(JsonAutoDetect.Visibility.NONE)
			.withCreatorVisibility(JsonAutoDetect.Visibility.NONE));
		try {
			String yaml = mapper.writeValueAsString(this);
			Files.writeString(prefsPath, yaml);
		} catch (IOException e) {
			log.error("Encountered exception while trying to write preferences", e);
		}
	}

}
