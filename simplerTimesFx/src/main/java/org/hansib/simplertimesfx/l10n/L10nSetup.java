/**
 * Abakus - https://github.com/hansi-b/SimplerTimesFx
 *
 * Copyright (C) 2022-2023 Hans Bering
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
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