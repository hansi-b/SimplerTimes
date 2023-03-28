/**
 * SimplerTimes - https://github.com/hansi-b/SimplerTimesFx
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
package org.hansib.simplertimes;

import java.util.Optional;

import org.hansib.simplertimes.fx.SimplerTimesFx;
import org.hansib.sundries.prefs.OptEnum;
import org.hansib.sundries.prefs.Prefs;
import org.hansib.sundries.prefs.ReqBoolean;
import org.hansib.sundries.prefs.store.PrefsStore;
import org.hansib.sundries.prefs.store.UserNodePrefsStore;

public class AppPrefs {

	private static PrefsStore fixedPrefsStore;

	/**
	 * poor person's dependency injection for a prefs store to use mocked prefs in
	 * tests
	 */
	static void fix(PrefsStore prefsStore) {
		fixedPrefsStore = prefsStore;
	}

	enum PrefKeys {
		_prefsVersion, // optionalEnum
		isDisclaimerAccepted; // requiredBoolean
	}

	enum PrefVersion {
		v1
	}

	public static AppPrefs create() {
		Prefs<PrefKeys> prefs = buildPrefs(
				fixedPrefsStore != null ? fixedPrefsStore : UserNodePrefsStore.forApp(SimplerTimesFx.class));
		return new AppPrefs(prefs);
	}

	private static Prefs<PrefKeys> buildPrefs(PrefsStore prefsStore) {
		return new Prefs.Builder<>(PrefKeys.class, prefsStore) //
				.optionalEnum(PrefKeys._prefsVersion, PrefVersion.class)//
				.requiredBoolean(PrefKeys.isDisclaimerAccepted, false)//
				.build();
	}

	private static final PrefVersion currentVersion = PrefVersion.v1;
	private final Prefs<PrefKeys> prefs;

	private AppPrefs(Prefs<PrefKeys> prefs) {

		this.prefs = prefs;
		ensureVersion();
	}

	/**
	 * Check our prefs for version information and update if necessary.
	 */
	private void ensureVersion() {

		OptEnum<PrefVersion> version = prefs.getPref(PrefKeys._prefsVersion);
		Optional<PrefVersion> incomingVersion = version.get();
		if (!incomingVersion.isPresent()) {
			version.set(currentVersion);
		} else {
			if (incomingVersion.get() != currentVersion)
				throw new IllegalStateException(String.format("Cannot handle preferences version %s (need %s)",
						incomingVersion.get(), currentVersion));
		}
	}

	public ReqBoolean disclaimerAccepted() {
		return prefs.getPref(PrefKeys.isDisclaimerAccepted);
	}
}
