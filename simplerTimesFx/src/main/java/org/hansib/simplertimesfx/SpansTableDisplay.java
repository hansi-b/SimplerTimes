package org.hansib.simplertimesfx;

import java.util.function.Supplier;

import org.hansib.simplertimes.spans.SpansCollection;
import org.hansib.sundries.fx.ButtonDecorator;
import org.hansib.sundries.fx.FxResourceLoader;

import javafx.scene.control.Button;
import javafx.stage.Stage;

class SpansTableDisplay {

	private final Supplier<SpansCollection> spansSupplier;
	private Stage spansStage;
	private SpansTableController spansTableController;

	SpansTableDisplay(Button showSpansButton, Supplier<SpansCollection> spansSupplier) {

		this.spansSupplier = spansSupplier;

		new ButtonDecorator(showSpansButton).graphic(Icons.showSpans()).onAction(event -> showSpansTable());
	}

	private void showSpansTable() {
		if (spansStage == null) {
			spansStage = new Stage();
			spansTableController = new FxResourceLoader().loadFxmlToStage("spansTable.fxml", spansStage);
		}
		spansTableController.setSpans(spansSupplier.get());
		spansStage.show();
	}
}