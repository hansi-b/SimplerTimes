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

import java.io.IOException;

import javafx.application.Platform;
import javafx.scene.control.Alert.AlertType;
import javafx.scene.control.ButtonType;
import javafx.scene.control.TextArea;
import javafx.scene.layout.Priority;
import javafx.scene.layout.VBox;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hansib.simplertimes.fx.l10n.Buttons;
import org.hansib.sundries.ResourceLoader;
import org.hansib.sundries.fx.AlertBuilder;
import org.hansib.sundries.prefs.PrimitiveBooleanPref;

class DisclaimerChecker {

	private static final Logger log = LogManager.getLogger();

	private final ResourceLoader resourceLoader = new ResourceLoader();

	static void checkDisclaimer(PrimitiveBooleanPref isDisclaimerAccepted, Runnable exitCall) {
		if (isDisclaimerAccepted.isTrue())
			return;

		Platform.runLater(() -> {
			boolean displayDisclaimerAndAccept = new DisclaimerChecker().askAcceptDisclaimer();
			isDisclaimerAccepted.set(displayDisclaimerAndAccept);
			if (!displayDisclaimerAndAccept) {
				log.info("Disclaimer was rejected");
				exitCall.run();
			}
		});
	}

	private boolean askAcceptDisclaimer() {
		log.trace("#showDisclaimer");

		final String disclaimer = loadDisclaimer();
		if (disclaimer == null)
			return false;

		final String doYouAccept = "Do you accept this agreement?\n(\"Cancel\" quits the program.)";
		TextArea textArea = new TextArea("%s%n%s".formatted(disclaimer, doYouAccept));
		textArea.setEditable(false);
		textArea.setWrapText(true);
		textArea.setPrefHeight(300);
		VBox.setVgrow(textArea, Priority.ALWAYS);

		return new AlertBuilder(AlertType.CONFIRMATION, new VBox(textArea)) //
				.withTitle("SimplerTimes - Disclaimer") //
				.withHeaderText("SimplerTimes - Disclaimer") //
				.withDefaultButton(ButtonType.CANCEL, Buttons.Cancel.fmt()) //
				.withButton(ButtonType.OK, Buttons.Ok.fmt()) //
				.resizable(true) //
				.showAndWaitFor(ButtonType.OK);
	}

	private String loadDisclaimer() {
		try {
			return resourceLoader.getResourceAsString("disclaimer.txt");
		} catch (final RuntimeException | IOException e) {
			log.error("Could not load disclaimer", e);
			new AlertBuilder(AlertType.ERROR,
					"The disclaimer could not be loaded: %s".formatted(e.getMessage())).withTitle(
							"Internal error while loading the disclaimer") //
					.showAndWait();
			return null;
		}
	}
}