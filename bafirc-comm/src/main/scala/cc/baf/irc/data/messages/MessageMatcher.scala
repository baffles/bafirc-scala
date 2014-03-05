package cc.baf.irc
package data.messages

import cc.baf.irc.data.Message

/**
 * Basic helper to make writing matchers for messages a little more concise.
 *
 * @author robertf
 */
abstract class MessageMatcher[T] {
	def matcher: PartialFunction[Message, T]

	def unapply(message: Message): Option[T] = matcher lift message
}