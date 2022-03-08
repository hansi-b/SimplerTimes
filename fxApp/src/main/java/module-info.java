module org.hansib.javaFxApp {
	requires org.hansib.javaFxApp.utilities;
	requires org.apache.logging.log4j;

	requires transitive javafx.controls;
	requires javafx.fxml;
	requires javafx.graphics;

	exports javafx_app.app;
}