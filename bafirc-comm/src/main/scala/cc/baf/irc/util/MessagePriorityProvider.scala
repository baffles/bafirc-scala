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
		def assignPriority(message: Message) = message match {
			case _ => 1
		}
	}
}