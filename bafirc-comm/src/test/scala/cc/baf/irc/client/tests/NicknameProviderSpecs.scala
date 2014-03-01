package cc.baf.irc
package client.tests

import scala.util._
import org.scalatest._

import cc.baf.irc.client._

class NicknameProviderSpecs extends FunSpec with Matchers {
	describe("BasicCurrentNick mixin") {
		it("should record and acknowledge new nicknames") {
			val bcn = new BasicCurrentNick {}
			(bcn.currentNick = "test") shouldEqual "test"
		}

		it("should track currentNick properly") {
			val bcn = new BasicCurrentNick {}

			bcn.currentNick = "test"
			bcn.currentNick shouldEqual "test"

			bcn.currentNick = "test2"
			bcn.currentNick shouldEqual "test2"
		}
	}

	describe("List-based Nickname Provider") {
		it("should report the nicknames in order") {
			val lnp = new ListNicknameProvider("test1" :: "test2" :: Nil)
			lnp.nextNick shouldEqual "test1"
			lnp.nextNick shouldEqual "test2"
		}

		it("should report the last used nickname, if it was changed") {
			val lnp = new ListNicknameProvider("test1" :: "test2" :: Nil)
			lnp.currentNick = "other"
			lnp.nextNick shouldEqual "other"
		}

		it("should, after changed nick was reported, move on to the listed nicks") {
			val lnp = new ListNicknameProvider("test1" :: "test2" :: Nil)
			lnp.currentNick = "other"
			lnp.nextNick
			lnp.nextNick shouldEqual "test1"
		}

		it("should move on from any listed nick that is changed to") {
			val lnp = new ListNicknameProvider("test1" :: "test2" :: "test3" :: Nil)
			lnp.currentNick = "test2"
			lnp.nextNick shouldEqual "test3"
		}

		it("should wrap around properly when it runs out of nicks") {
			val lnp = new ListNicknameProvider("test1" :: "test2" :: Nil)
			lnp.nextNick shouldEqual "test1"
			lnp.nextNick shouldEqual "test2"
			lnp.nextNick shouldEqual "test1"
		}
	}

	describe("Generated Nickname Provider") {
		val ValidNick = """[A-Za-z\[\]\\`_\^{\|}][A-Za-z\d\[\]\\`_\^{\|}\-]*""".r

		it("should start with the initial nickname") {
			val gnp = new GeneratedNicknameProvider("test")
			gnp.nextNick shouldEqual "test"
		}

		it("should generate a new valid nickname sequentially") {
			val gnp = new GeneratedNicknameProvider("test")
			gnp.nextNick
			gnp.nextNick should fullyMatch regex ValidNick
		}

		it("should report the last used nickname, if it was changed") {
			val gnp = new GeneratedNicknameProvider("test")
			gnp.currentNick = "other"
			gnp.nextNick shouldEqual "other"
		}

		it("should, after changed nick was reported, generate nicks based on that") {
			val gnp = new GeneratedNicknameProvider("test")
			gnp.currentNick = "other"
			gnp.nextNick
			val next = gnp.nextNick
			next should not equal ("other")
			next should fullyMatch regex ValidNick
		}

		it("should continually generate new, valid, nicks") {
			val gnp = new GeneratedNicknameProvider("sT6")
			val nicks = for (_ <- 1 to 50) yield {
				val n = gnp.nextNick
				n should fullyMatch regex ValidNick
				n
			}
			nicks.map(_.toLowerCase).distinct.length shouldEqual 50
		}
	}
}