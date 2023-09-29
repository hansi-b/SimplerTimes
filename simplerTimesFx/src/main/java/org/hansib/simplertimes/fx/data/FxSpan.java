/**
 * SimplerTimes - https://github.com/hansi-b/SimplerTimes
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
package org.hansib.simplertimes.fx.data;

import java.time.Duration;
import java.time.OffsetDateTime;

import org.hansib.simplertimes.spans.Span;

import javafx.beans.binding.Bindings;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.ReadOnlyObjectProperty;
import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.property.SimpleObjectProperty;

public class FxSpan {

	private final SimpleObjectProperty<FxProject> fxProject;

	private final SimpleObjectProperty<OffsetDateTime> start;
	private final SimpleObjectProperty<OffsetDateTime> end;
	private final ReadOnlyObjectWrapper<Duration> duration;

	public FxSpan(FxProject project, OffsetDateTime start, OffsetDateTime end) {
		this.fxProject = new SimpleObjectProperty<>(project);

		this.start = new SimpleObjectProperty<>(start);
		this.end = new SimpleObjectProperty<>(end);

		this.duration = new ReadOnlyObjectWrapper<>();
		duration.bind(Bindings.createObjectBinding(() -> Duration.between(this.start.get(), this.end.get()), this.start,
				this.end));
	}

	public ObjectProperty<FxProject> fxProject() {
		return fxProject;
	}

	public SimpleObjectProperty<OffsetDateTime> start() {
		return start;
	}

	public SimpleObjectProperty<OffsetDateTime> end() {
		return end;
	}

	public ReadOnlyObjectProperty<Duration> duration() {
		return duration;
	}

	Span toSpan() {
		return new Span(fxProject().get().project(), start().get(), end().get());
	}

	@Override
	public String toString() {
		return "[%s -> %s: %s]".formatted(start.get(), end.get(), fxProject.get());
	}
}