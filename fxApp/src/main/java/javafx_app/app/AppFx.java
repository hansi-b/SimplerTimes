package javafx_app.app;

import java.util.Arrays;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javafx.application.Application;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.layout.StackPane;
import javafx.stage.Stage;
import javafx_app.utilities.StringUtils;

public class AppFx extends Application {

	private static final Logger log = LogManager.getLogger();

	@Override
	public void start(Stage primaryStage) throws Exception {

		primaryStage.setTitle("AppFx");
		Button btn = new Button();
		btn.setText("Say 'Hello World'");
		btn.setOnAction(event -> log.info(StringUtils.join(Arrays.asList("Hello World!".split("\s")))));

		StackPane root = new StackPane();
		root.getChildren().add(btn);
		primaryStage.setScene(new Scene(root, 300, 250));
		primaryStage.show();
	}

	public static void main(String[] args) {
		launch(AppFx.class, args);
	}
}