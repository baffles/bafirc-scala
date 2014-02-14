package cc.baf.irc
package parser.tests

import scala.util._
import org.scalatest._

import cc.baf.irc.data._
import cc.baf.irc.parser._

class IrcParserSpec extends FunSpec with Matchers {

	private lazy val parser = new IrcParser()
	
	private def parseSuccess(message: String) = {
		parser.tryParseMessage(message) match {
			case Success(parsed) => parsed
			case Failure(exception) => fail(s"Message failed to parse: $exception")
		}
	}

	describe("Message Parser") {
		it("should properly parse message prefixes") {
			val all = parseSuccess(":nick!user@host COMMAND param1 param2 :param3")
			all.prefix shouldEqual Some(Prefix("nick", Some("user"), Some("host")))

			val nickOnly = parseSuccess(":nick COMMAND param1 param2 :param3\r\n")
			nickOnly.prefix shouldEqual Some(Prefix("nick", None, None))

			val nickUserOnly = parseSuccess(":nick!user COMMAND param1 param2 :param3")
			nickUserOnly.prefix shouldEqual Some(Prefix("nick", Some("user"), None))

			val nickHostOnly = parseSuccess(":nick@host COMMAND param1 param2 :param3")
			nickHostOnly.prefix shouldEqual Some(Prefix("nick", None, Some("host")))
		}

		it("shouldn't fail on messages with no prefix") {
			val noPrefix = parseSuccess("COMMAND param1 param2")
			noPrefix.prefix shouldEqual None
		}

		it("should properly parse command from messages") {
			val parsed = parseSuccess("COMMAND param")
			parsed.command shouldEqual "COMMAND"
		}

		it("should upper-case commands in messages") {
			val parsed = parseSuccess("command param")
			parsed.command shouldEqual "COMMAND"
		}

		it("should properly parse middle-params") {
			val oneparam = parseSuccess("COMMAND param1")
			oneparam.params shouldEqual "param1" :: Nil

			val fourparams = parseSuccess("COMMAND param1 param2 param3 param4")
			fourparams.params shouldEqual "param1" :: "param2" :: "param3" :: "param4" :: Nil
		}

		it("should properly parse trailing-params") {
			val noMiddle = parseSuccess("COMMAND :trailing param")
			noMiddle.params shouldEqual "trailing param" :: Nil

			val middle = parseSuccess("COMMAND param1 :trailing trailing:trail")
			middle.params shouldEqual "param1" :: "trailing trailing:trail" :: Nil
		}

		it("should handle (i.e., throw away) CRLF at end of messages") {
			val crlf = parseSuccess("COMMAND :trailing\r\n")
			crlf.params shouldEqual "trailing" :: Nil
		}

		it("should handle any number of spaces after prefix") {
			var twoSpacesAfterPrefix = parseSuccess(":prefix  COMMAND param")
			twoSpacesAfterPrefix.prefix shouldEqual Some(Prefix("prefix", None, None))
			twoSpacesAfterPrefix.command shouldEqual "COMMAND"

			var threeSpacesAfterPrefix = parseSuccess(":prefix   COMMAND param")
			threeSpacesAfterPrefix.prefix shouldEqual Some(Prefix("prefix", None, None))
			threeSpacesAfterPrefix.command shouldEqual "COMMAND"
		}

		it("should handle any number of spaces after command") {
			var twoSpacesAfterCommand = parseSuccess(":prefix COMMAND  param")
			twoSpacesAfterCommand.command shouldEqual "COMMAND"
			twoSpacesAfterCommand.params shouldEqual "param" :: Nil

			var threeSpacesAfterCommand = parseSuccess(":prefix COMMAND   param")
			threeSpacesAfterCommand.command shouldEqual "COMMAND"
			threeSpacesAfterCommand.params shouldEqual "param" :: Nil
		}

		it("should handle any number of spaces between middle-params") {
			var twoSpacesBetweenParams = parseSuccess("COMMAND param1  param2")
			twoSpacesBetweenParams.params shouldEqual "param1" :: "param2" :: Nil

			var threeSpacesBetweenParams = parseSuccess("COMMAND param1   param2")
			threeSpacesBetweenParams.params shouldEqual "param1" :: "param2" :: Nil
		}

		it("should handle any number of spaces between middle-param and trailing-param") {
			var twoSpacesBeforeTrailing = parseSuccess("COMMAND param1  :param2")
			twoSpacesBeforeTrailing.params shouldEqual "param1" :: "param2" :: Nil

			var threeSpacesBeforeTrailing = parseSuccess("COMMAND param1   :param2")
			threeSpacesBeforeTrailing.params shouldEqual "param1" :: "param2" :: Nil
		}

		it("should handle any number of spaces between command and trailing-param") {
			var twoSpacesBeforeTrailing = parseSuccess("COMMAND  :param")
			twoSpacesBeforeTrailing.command shouldEqual "COMMAND"
			twoSpacesBeforeTrailing.params shouldEqual "param" :: Nil

			var threeSpacesBeforeTrailing = parseSuccess("COMMAND   :param")
			threeSpacesBeforeTrailing.command shouldEqual "COMMAND"
			threeSpacesBeforeTrailing.params shouldEqual "param" :: Nil
		}

		it("should handle commands without parameters properly") {
			val noParams = parseSuccess("QUIT")
			noParams.command shouldEqual "QUIT"
			noParams.params shouldEqual Nil
		}
	}
}