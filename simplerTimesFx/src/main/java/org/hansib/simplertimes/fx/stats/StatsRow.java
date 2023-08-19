package org.hansib.simplertimes.fx.stats;

import java.time.Duration;
import java.time.LocalDate;
import java.util.HashMap;
import java.util.Map;

import org.hansib.simplertimes.fx.data.FxProject;
import org.hansib.simplertimes.times.Utils;

import javafx.beans.property.ObjectProperty;
import javafx.beans.property.ReadOnlyObjectWrapper;

record StatsRow(ObjectProperty<String> project, Map<LocalDate, ObjectProperty<String>> durations) {

	static StatsRow of(FxProject p, Map<LocalDate, Duration> durations) {
		Map<LocalDate, ObjectProperty<String>> res = new HashMap<>();
		durations.forEach((odt, d) -> res.put(odt, new ReadOnlyObjectWrapper<>(Utils.toHmsString(d))));
		return new StatsRow(new ReadOnlyObjectWrapper<>(p.text()), res);
	}

	ObjectProperty<String> ldStr(LocalDate odt) {
		return durations.get(odt);
	}
}
