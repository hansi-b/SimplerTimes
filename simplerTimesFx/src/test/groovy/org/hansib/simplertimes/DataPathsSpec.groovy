package org.hansib.simplertimes;

import java.nio.file.Path

import spock.lang.Specification
import spock.lang.TempDir

public class DataPathsSpec extends Specification {

	@TempDir
	Path testDir

	def "can create app data in existing dir"() {

		expect:
		DataPaths.at(testDir).spansPath() == testDir.resolve("spans.yml")
	}

	def "can create app data in new dir"() {

		expect:
		DataPaths.at(testDir.resolve("does_not_exist")).spansPath() == testDir.resolve("does_not_exist/spans.yml")
	}

	def "app data creation fails if target is file"() {

		given:
		def tmpFilePath = testDir.resolve("tmpFile")
		tmpFilePath.toFile().createNewFile()

		when:
		DataPaths.at(tmpFilePath)
		then:
		thrown IllegalArgumentException
	}
}
