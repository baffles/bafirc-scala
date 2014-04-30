package cc.baf.irc
package util

import scala.math.Ordering

import cc.baf.irc.data.Message

/**
 * Provider for message ordering/prioritization to determine what gets sent first when messages are being queued by the throttler.
 * @author robertf
 */
trait MessagePriorityProvider {
	/** Lower priority values are sent first. Priority 1 > Priority 2, etc. */
	def assignPriority(message: Message): Int

	implicit object MessageOrdering extends Ordering[Message] {
		def compare(x: Message, y: Message) = -(assignPriority(x) compare assignPriority(y))
	}
}

object MessagePriorityProvider {
	/** Default priority implementation */
	object DefaultProvider extends MessagePriorityProvider {
		private val HighPriority = Set("PONG")
		private val LowPriority = Set("NOTICE", "PRIVMSG")

		def assignPriority(message: Message) = message match {
			case message if HighPriority contains message.command.toUpperCase => 1
			case message if (message.command.toUpperCase == "PRIVMSG") && (message.params(1) contains "hpri") => 1
			case message if LowPriority contains message.command.toUpperCase => 3
			case _ => 2
		}
	}
}