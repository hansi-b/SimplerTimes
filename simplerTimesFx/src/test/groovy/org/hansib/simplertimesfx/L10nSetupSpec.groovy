package org.hansib.simplertimesfx;

import java.util.function.Consumer

import org.hansib.sundries.l10n.L10nChecker
import org.hansib.sundries.l10n.L10nChecker.MissingKeys
import org.hansib.sundries.l10n.L10nChecker.MissingKeysHandleMode

import spock.lang.Specification

public class L10nSetupSpec extends Specification {

	def 'english is complete' () {
		when:
		def errors = [] as List
		Consumer<MissingKeys<?>> errCollector = k -> errors.add(k)
		def english = L10nSetup.loadEnglish(errCollector)
		then:
		assert errors == [] as List, "Errors:\n" + errors.collect {
			it.description()
		}. join("\n")

		when:
		def missing = [] as List
		Consumer<MissingKeys<?>> missesCollector = k -> missing.add(k)
		new L10nChecker(english).checkCompleteness(missesCollector, MissingKeysHandleMode.OnlyWithMissingKeys)
		then:
		assert missing == [] as List, "Missing:\n" + missing.collect { m ->
			"${m.enumClz.simpleName}: ${m.missing}"
		}. join("\n")
	}
}
