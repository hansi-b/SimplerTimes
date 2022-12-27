package org.hansib.simplertimesfx;

import java.io.IOException;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hansib.sundries.ResourceLoader;
import org.hansib.sundries.l10n.Domain;
import org.hansib.sundries.l10n.FormatKey;
import org.hansib.sundries.l10n.L10n;

enum L10nKeys implements FormatKey {
	AppNameWindowTitle
}

enum MenuItems implements FormatKey {
	NewProject, //
	NewSubproject, //
	Exit
}

class L10nSetup {
	private static final Logger log = LogManager.getLogger();

	private static final String englishYaml = "english.yaml";

	private L10nSetup() {
		// nothing to do
	}

	static void activateEnglish() {
		L10n english = new L10n(new Domain().with(L10nKeys.class).with(MenuItems.class));

		String l10nYaml;
		try {
			l10nYaml = new ResourceLoader().getResourceAsString(englishYaml);
		} catch (IOException ex) {
			log.error("Exception while trying to load", ex);
			return;
		}

		log.info("Loading L10n from '{}' ...", englishYaml);
		english.load(l10nYaml, e -> log.warn(e.description()));
		english.activate();
	}
}