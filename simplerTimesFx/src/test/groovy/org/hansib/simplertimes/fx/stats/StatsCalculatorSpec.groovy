package org.hansib.simplertimes.fx.stats

import java.time.Duration
import java.time.LocalDate
import java.time.LocalTime
import java.time.OffsetDateTime
import java.time.ZoneOffset

import org.hansib.simplertimes.fx.data.FxProject
import org.hansib.simplertimes.fx.data.FxSpan

import javafx.collections.FXCollections
import javafx.collections.ObservableList
import spock.lang.Shared
import spock.lang.Specification

public class StatsCalculatorSpec extends Specification {

	@Shared ZoneOffset zoneOffset = ZoneOffset.ofHours(2)

	@Shared FxProject pro_A = Mock()
	@Shared FxProject pro_B = Mock()

	@Shared LocalDate ld_07_20 = ld(2023, 7, 20)
	@Shared LocalDate ld_07_21 = ld(2023, 7, 21)

	private SortedSet<LocalDate> dates = [] as SortedSet
	private ObservableList<FxSpan> spans = FXCollections.observableArrayList()
	private StatsCalculator calc

	def setupSpec() {
		pro_A.text() >> 'project A'
		pro_B.text() >> 'project B'
	}

	def 'can calculate stats' () {

		given:
		def dura_1 = Duration.ofMinutes(5).plusSeconds(30)
		def dura_2 = Duration.ofMinutes(6).plusSeconds(40)

		spans.add(fxSpan(pro_A, odt(ld_07_20, lt(10, 40)), dura_1))
		spans.add(fxSpan(pro_A, odt(ld_07_20, lt(20, 00)), dura_2))
		calc = new StatsCalculator(spans)

		when:
		dates.add(ld_07_20)
		def rows = calc.calcItems(dates)

		then:
		rows.size() == 1
		rows.get(0).equalsByValues(StatsRow.of(pro_A, [(ld_07_20) : dura_1.plus(dura_2)]))
	}

	private OffsetDateTime odt(LocalDate ld, LocalTime lt) {
		OffsetDateTime.of(ld, lt, zoneOffset)
	}

	private static LocalTime lt(int hour, int minute) {
		LocalTime.of(hour, minute)
	}

	private static LocalDate ld(int year, int month, int day) {
		LocalDate.of(year, month, day)
	}

	private FxSpan fxSpan(FxProject project, OffsetDateTime start, Duration d) {
		return new FxSpan(project, start, start.plus(d))
	}
}
