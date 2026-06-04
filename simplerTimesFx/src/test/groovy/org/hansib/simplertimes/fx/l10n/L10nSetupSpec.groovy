package org.hansib.simplertimes.fx.l10n;

import org.hansib.sundries.l10n.L10nChecker
import org.hansib.sundries.l10n.L10nChecker.MissingKeys
import org.hansib.sundries.l10n.L10nChecker.MissingKeysHandleMode
import org.hansib.sundries.l10n.yaml.errors.L10nFormatError
import spock.lang.Specification

public class L10nSetupSpec extends Specification {

	def 'english is complete' () {
		when:
		def errors = []
		def english = L10nSetup.load(L10nSetup.Locale.en, k -> errors.add(k))
		then:
		assert errors == [], "Errors:\n" + errors.collect { L10nFormatError e ->
			e.description()
		}. join("\n")

		when:
		def missing = []
		new L10nChecker(english).checkCompleteness(k -> missing.add(k), MissingKeysHandleMode.OnlyWithMissingKeys)
		then:
		assert missing == [], "Missing:\n" + missing.collect { MissingKeys<?> m ->
			m.description()
		}. join("\n")
	}

	def 'german is complete' () {
		when:
		def errors = []
		def german = L10nSetup.load(L10nSetup.Locale.de, k -> errors.add(k))
		then:
		assert errors == [], "Errors:\n" + errors.collect { L10nFormatError e ->
			e.description()
		}. join("\n")

		when:
		def missing = []
		new L10nChecker(german).checkCompleteness(k -> missing.add(k), MissingKeysHandleMode.OnlyWithMissingKeys)
		then:
		assert missing == [], "Missing:\n" + missing.collect { MissingKeys<?> m ->
			m.description()
		}. join("\n")
	}

	def 'french is complete' () {
		when:
		def errors = []
		def french = L10nSetup.load(L10nSetup.Locale.fr, k -> errors.add(k))
		then:
		assert errors == [], "Errors:\n" + errors.collect { L10nFormatError e ->
			e.description()
		}. join("\n")

		when:
		def missing = []
		new L10nChecker(french).checkCompleteness(k -> missing.add(k), MissingKeysHandleMode.OnlyWithMissingKeys)
		then:
		assert missing == [], "Missing:\n" + missing.collect { MissingKeys<?> m ->
			m.description()
		}. join("\n")
	}
}
