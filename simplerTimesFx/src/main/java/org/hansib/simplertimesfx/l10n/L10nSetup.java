package org.hansib.simplertimesfx.l10n;

import java.io.IOException;
import java.util.function.Consumer;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hansib.sundries.ResourceLoader;
import org.hansib.sundries.l10n.Domain;
import org.hansib.sundries.l10n.L10n;
import org.hansib.sundries.l10n.yaml.errors.L10nFormatError;

public class L10nSetup {
	private static final Logger log = LogManager.getLogger();

	private static final String englishYaml = "english.yaml";

	private L10nSetup() {
		// nothing to do
	}

	public static L10n activateEnglish() {
		L10n english = loadEnglish(e -> log.warn(e::description));
		english.activate();
		return english;
	}

	private static L10n loadEnglish(Consumer<L10nFormatError> errorHandler) {
		L10n english = new L10n(new Domain().with(L10nKeys.class).with(MenuItems.class));
		try {
			String l10nYaml = new ResourceLoader().getResourceAsString(englishYaml);
			log.info("Loading L10n from '{}' ...", englishYaml);
			english.load(l10nYaml, errorHandler);
		} catch (IOException ex) {
			log.error("Exception while trying to load L10n from '{}'", englishYaml);
			log.error(ex);
		}
		return english;
	}
}