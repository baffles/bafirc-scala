package cc.baf.irc
package data.messages.tests

import org.scalatest._

import cc.baf.irc.data._
import cc.baf.irc.data.messages._

class MessageSpec extends FunSpec with Matchers {

	//describe("Join Message")

	describe("Part Message") {
		describe("Generator") {
			it("should generate a single-channel part correctly") {
				val msg = Part("#channel")
				msg shouldEqual Message(None, "PART", List("#channel"))
			}

			it("should generate a single-channel part with message correctly") {
				val msg = Part("#channel", "leaving")
				msg shouldEqual Message(None, "PART", List("#channel", "leaving"))
			}

			it("should generate a multi-channel part correctly") {
				val msg = Part(List("#c1", "#c2"))
				msg shouldEqual Message(None, "PART", List("#c1,#c2"))
			}

			it("should generate a multi-channel part with message correctly") {
				val msg = Part(List("#c1", "#c2"), "bye")
				msg shouldEqual Message(None, "PART", List("#c1,#c2", "bye"))
			}
		}

		describe("Matcher") {
			it("should recognize a part with no message") {
				val user = Prefix("test", None, None)
				val msg = Message(user, "PART", List("#channel"))
				Part.unapply(msg) shouldEqual Some((user, "#channel", None))
			}

			it("should recognize a part with a message") {
				val user = Prefix("test", None, None)
				val msg = Message(user, "PART", List("#channel", "leaving"))
				Part.unapply(msg) shouldEqual Some((user, "#channel", Some("leaving")))
			}
		}
	}

	describe("Topic Message") {
		describe("Generator") {
			it("should generate a topic query correctly") {
				val msg = Topic("#channel")
				msg shouldEqual Message(None, "TOPIC", List("#channel"))
			}

			it("should generate a topic set message correctly") {
				val msg = Topic("#channel", "new topic")
				msg shouldEqual Message(None, "TOPIC", List("#channel", "new topic"))
			}

			it("should generate a topic clear message correctly") {
				val msg = Topic("#channel", None)
				msg shouldEqual Message(None, "TOPIC", List("#channel", ""))
			}
		}

		describe("Matcher") {
			it("should recognize a topic set message") {
				val user = Prefix("test", None, None)
				val msg = Message(user, "TOPIC", List("#channel", "my topic"))
				Topic.unapply(msg) shouldEqual Some((user, "#channel", Some("my topic")))
			}

			it("should recognize a topic clear message") {
				val user = Prefix("test", None, None)
				val msg = Message(user, "TOPIC", List("#channel", ""))
				Topic.unapply(msg) shouldEqual Some((user, "#channel", None))
			}
		}
	}
}