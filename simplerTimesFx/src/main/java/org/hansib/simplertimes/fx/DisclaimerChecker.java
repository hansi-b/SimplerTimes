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
package org.hansib.simplertimes.fx;

import javafx.application.Platform;
import javafx.scene.control.Alert.AlertType;
import javafx.scene.control.ButtonType;
import javafx.scene.control.TextArea;
import javafx.scene.layout.Priority;
import javafx.scene.layout.VBox;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hansib.simplertimes.fx.l10n.Buttons;
import org.hansib.simplertimes.fx.l10n.Disclaimer;
import org.hansib.simplertimes.prefs.Prefs;
import org.hansib.sundries.fx.AlertBuilder;

class DisclaimerChecker {

  private static final Logger log = LogManager.getLogger();

  static void checkDisclaimer(Prefs.Disclaimer disclaimer, Runnable exitCall) {
    if (disclaimer.isAccepted) return;

    Platform.runLater(
        () -> {
          boolean displayDisclaimerAndAccept = new DisclaimerChecker().askAcceptDisclaimer();
          disclaimer.isAccepted = displayDisclaimerAndAccept;
          if (!displayDisclaimerAndAccept) {
            log.info("Disclaimer was rejected");
            exitCall.run();
          }
        });
  }

  private boolean askAcceptDisclaimer() {
    log.trace("#showDisclaimer");

    TextArea textArea = new TextArea("%s%n%s".formatted(Disclaimer.Text.fmt(), Disclaimer.Question.fmt()));
    textArea.setEditable(false);
    textArea.setWrapText(true);
    textArea.setPrefHeight(300);
    VBox.setVgrow(textArea, Priority.ALWAYS);

    final String title = Disclaimer.Title.fmt();
    return new AlertBuilder(AlertType.CONFIRMATION, new VBox(textArea))
        .withTitle(title)
        .withHeaderText(title)
        .withDefaultButton(ButtonType.CANCEL, Buttons.Cancel.fmt())
        .withButton(ButtonType.OK, Buttons.Ok.fmt())
        .resizable(true)
        .showAndWaitFor(ButtonType.OK);
  }
}
