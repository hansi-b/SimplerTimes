package org.hansib.simplertimes.fx.stats;

import java.time.Duration
import java.time.LocalDate

import org.hansib.simplertimes.fx.data.FxProject

import javafx.beans.property.ReadOnlyStringWrapper
import spock.lang.Shared
import spock.lang.Specification

public class StatsRowSpec extends Specification {

	@Shared FxProject proj_A = Mock()
	@Shared FxProject proj_B = Mock()

	@Shared def ld_07_20 = LocalDate.of(2023, 7, 20)
	@Shared def ld_07_21 = LocalDate.of(2023, 7, 21)

	@Shared def dura_1 = Duration.ofMinutes(1)
	@Shared def dura_3 = Duration.ofMinutes(3)

	def setupSpec() {
		ReadOnlyStringWrapper proj_A_name = new ReadOnlyStringWrapper('project A')
		proj_A.name() >> proj_A_name
		ReadOnlyStringWrapper proj_B_name = new ReadOnlyStringWrapper('project B')
		proj_B.name() >> proj_B_name
	}

	def 'equalsByValues for simple cases' () {

		given:
		def row_A = StatsRow.of(proj_A, [(ld_07_20) : dura_1])

		expect:
		row_A.equalsByValues(row_A) == true
		row_A.equalsByValues(null) == false
		row_A.equalsByValues(StatsRow.of(proj_A, [(ld_07_20) : dura_1])) == true
	}

	def 'equalsByValues for various differences' () {

		given:
		def row_A = StatsRow.of(proj_A, [(ld_07_20) : dura_1])
		def row_B = StatsRow.of(proj_A, [(ld_07_20) : dura_1, (ld_07_21) : dura_1])

		expect:
		row_A.equalsByValues(StatsRow.of(proj_B, [(ld_07_20) : dura_1])) == false
		row_A.equalsByValues(StatsRow.of(proj_A, [(ld_07_21) : dura_1])) == false
		row_A.equalsByValues(StatsRow.of(proj_A, [(ld_07_20) : dura_3])) == false

		row_A.equalsByValues(row_B) == false
		row_B.equalsByValues(row_A) == false
	}
}
