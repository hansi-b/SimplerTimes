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
package org.hansib.simplertimes.fx;

import java.io.IOException;
import java.util.Optional;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hansib.sundries.ResourceLoader;
import org.hansib.sundries.prefs.PrimitiveBooleanPref;

import javafx.application.Platform;
import javafx.scene.control.Alert;
import javafx.scene.control.Alert.AlertType;
import javafx.scene.control.Button;
import javafx.scene.control.ButtonType;
import javafx.scene.control.TextArea;
import javafx.scene.layout.Priority;
import javafx.scene.layout.VBox;

class DisclaimerChecker {

	private static final Logger log = LogManager.getLogger();

	private final ResourceLoader resourceLoader = new ResourceLoader();

	static void checkDisclaimer(PrimitiveBooleanPref isDisclaimerAccepted, Runnable exitCall) {
		if (isDisclaimerAccepted.isTrue())
			return;

		Platform.runLater(() -> {
			boolean displayDislaimerAndAccept = new DisclaimerChecker().askAcceptDisclaimer();
			isDisclaimerAccepted.set(displayDislaimerAndAccept);
			if (!displayDislaimerAndAccept) {
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

		final String frage = "Akzeptieren Sie diese Nutzungsvereinbarung?\n(\"Nein\" schließt das Programm.)";
		final Alert disclaimerConf = new Alert(AlertType.CONFIRMATION, "", ButtonType.YES, ButtonType.NO);
		Button noButton = (Button) disclaimerConf.getDialogPane().lookupButton(ButtonType.NO);
		noButton.setText("Nein");
		noButton.setDefaultButton(true);
		Button yesButton = (Button) disclaimerConf.getDialogPane().lookupButton(ButtonType.YES);
		yesButton.setText("Ja");
		yesButton.setDefaultButton(false);

		disclaimerConf.setHeaderText("SimplerTimes - Nutzungsvereinbarung");
		disclaimerConf.setTitle("SimplerTimes - Nutzungsvereinbarung");

		TextArea textArea = new TextArea(String.format("%s%n%s", disclaimer, frage));
		textArea.setEditable(false);
		textArea.setWrapText(true);

		VBox.setVgrow(textArea, Priority.ALWAYS);

		VBox pane = new VBox();
		pane.getChildren().add(textArea);

		disclaimerConf.getDialogPane().setContent(pane);
		disclaimerConf.setResizable(true);

		final Optional<ButtonType> answer = disclaimerConf.showAndWait();
		return answer.isPresent() && answer.get().equals(ButtonType.YES);
	}

	private String loadDisclaimer() {
		try {
			return resourceLoader.getResourceAsString("disclaimer.txt");

		} catch (final RuntimeException | IOException e) {
			log.error("Could not load disclaimer", e);
			final Alert alert = new Alert(AlertType.ERROR,
					String.format("Die Nutzungsvereinbarung konnte nicht geladen werden: %s", e.getMessage()));
			alert.setTitle("Interner Fehler beim Laden der Nutzungsvereinbarung");
			alert.showAndWait();
			return null;
		}
	}
}