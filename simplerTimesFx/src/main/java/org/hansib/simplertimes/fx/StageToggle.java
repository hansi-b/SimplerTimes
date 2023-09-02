package org.hansib.simplertimes.fx;

import java.util.function.Supplier;

import javafx.stage.Stage;

public class StageToggle {

	private final Supplier<Stage> stageInit;
	private Stage stage;

	public StageToggle(Supplier<Stage> stageInit) {
		this.stageInit = stageInit;
	}

	/**
	 * Initialises stage on first call, toggles visibility afterwards.
	 */
	public void toggle() {
		if (stage == null) {
			stage = stageInit.get();
			stage.setOnShown(e -> {
				stage.setX(stage.getX());
				stage.setY(stage.getY());
				stage.setWidth(stage.getWidth());
				stage.setHeight(stage.getHeight());
			});
		}
		if (stage.isShowing())
			stage.hide();
		else
			stage.show();
	}
}