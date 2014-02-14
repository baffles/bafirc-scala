package cc.baf.irc
package data.tests

import org.scalatest._

import cc.baf.irc.data._

class MessageSpec extends FunSpec with Matchers {

	describe("Message Rendering") {
		describe("Message Prefixes") {
			it("should render <nick> correctly") {
				val prefix = Prefix("nick", None, None)
				prefix.toIrc shouldEqual ":nick "
			}

			it("should render <nick!user> correctly") {
				val prefix = Prefix("nick", Some("user"), None)
				prefix.toIrc shouldEqual ":nick!user "
			}

			it("should render <nick@host> correctly>") {
				val prefix = Prefix("nick", None, Some("host"))
				prefix.toIrc shouldEqual ":nick@host "
			}

			it("should render <nick!user@host> correctly") {
				val prefix = Prefix("nick", Some("user"), Some("host"))
				prefix.toIrc shouldEqual ":nick!user@host "
			}
		}

		describe("Message Content") {
			it("should render a basic non-prefixed non-parametered command") {
				val msg = Message("test")
				msg.toIrc shouldEqual "test"
			}

			it("should render middle parameters correctly") {
				val oneParam = Message("test", "param")
				oneParam.toIrc shouldEqual "test param"

				val twoParams = Message("test", "p1", "p2")
				twoParams.toIrc shouldEqual "test p1 p2"
			}

			it("should render trailing paramters (space) correctly") {
				val msg = Message("test", "trailing param")
				msg.toIrc shouldEqual "test :trailing param"
			}

			it("should render trailing parameters (beginning with colon) correctly") {
				val msg = Message("test", ":trailing")
				msg.toIrc shouldEqual "test ::trailing"
			}

			it("should render a command with middle and trailing parameters correctly") {
				val msg = Message("test", "m1", "m2", "trailing param")
				msg.toIrc shouldEqual "test m1 m2 :trailing param"
			}

			it("should throw on middle parameters with spaces") {
				val msg = Message("test", "middle with space", "trailing")
				intercept[IllegalArgumentException] {
					msg.toIrc
				}
			}

			it("should throw on middle parameters beginning with colons") {
				val msg = Message("test", ":middle", "trailing")
				intercept[IllegalArgumentException] {
					msg.toIrc
				}
			}
		}
	}

	it("should render a full prefixed message properly") {
		val msg = Message(Prefix("nick", Some("user"), Some("host")), "command", "p1", "p2", "p3", "p4 trailing")
		msg.toIrc shouldEqual ":nick!user@host command p1 p2 p3 :p4 trailing"
	}
}