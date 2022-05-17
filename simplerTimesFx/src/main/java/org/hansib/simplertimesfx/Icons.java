package org.hansib.simplertimesfx;

import org.kordamp.ikonli.javafx.FontIcon;

import javafx.scene.Node;

class Icons {

	static final Node start() {
		return new FontIcon("codicon-play-circle");
	}

	static final Node stop() {
		return new FontIcon("codicon-stop-circle");
	}

	static final Node editTree() {
		return new FontIcon("codicon-list-tree");
	}
}