package org.hansib.simplertimesfx;

import org.hansib.sundries.l10n.FormatKey;
import org.hansib.sundries.l10n.L10n;

enum L10nKeys implements FormatKey {
	AppNameWindowTitle, //
	MenuItemExit
}

class L10nSetup {
	static void activateEnglish() {
		L10n english = new L10n(L10nKeys.class) //
				.add(L10nKeys.AppNameWindowTitle, "SimplerTimes") //
				.add(L10nKeys.MenuItemExit, "Exit") //
		;

		english.activate();
	}
}