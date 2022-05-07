package org.hansib.simpler_times.utils;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.nio.charset.StandardCharsets;

import javafx.fxml.FXMLLoader;

public class ResourceLoader {

	public static ResourceLoader get() {
		return new ResourceLoader();
	}

	public String resourceAsString(final String resourceName) throws IOException {
		try (InputStream resStream = getResourceStream(resourceName)) {
			return new String(resStream.readAllBytes(), StandardCharsets.UTF_8);
		}
	}

	public InputStream getResourceStream(String resourceName) {
		return getClass().getClassLoader().getResourceAsStream(resourceName);
	}

	public FXMLLoader getFxmlLoader(String fxml) {
		return new FXMLLoader(getResourceUrl("fxml/" + fxml));
	}

	public URL getResourceUrl(String resName) {
		return getClass().getClassLoader().getResource(resName);
	}
}