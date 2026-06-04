/*-
 * GPL for SimplerTimes - https://github.com/hansi-b/SimplerTimes
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
package org.hansib.simplertimes.fx.l10n;

import java.util.function.Consumer;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hansib.sundries.l10n.L10n;
import org.hansib.sundries.l10n.yaml.errors.L10nFormatError;
import org.hansib.sundries.testing.VisibleForTesting;

public class L10nSetup {
  private static final Logger log = LogManager.getLogger();

  public static enum Locales {
    en,
    de,
    fr
  }

  private L10nSetup() {
    // nothing to do
  }

  public static L10n activate(Locales locale) {
    Consumer<L10nFormatError> errorHandler = e -> log.warn(e::description);
    L10n l10n = load(locale, errorHandler);
    l10n.activate();
    return l10n;
  }

  @VisibleForTesting
  static L10n load(Locales locale, Consumer<L10nFormatError> errorHandler) {
    L10n l10n = new L10n()
        .with(General.class)
        .with(MenuItems.class)
        .with(Names.class)
        .with(Buttons.class)
        .with(Headers.class)
        .with(Disclaimer.class);
    l10n.load("l10n", locale, errorHandler);
    return l10n;
  }

}
