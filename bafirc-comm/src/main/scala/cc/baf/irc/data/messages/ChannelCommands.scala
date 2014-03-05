package cc.baf.irc
package data.messages

import language.implicitConversions

import cc.baf.irc.data.{ Message, Prefix }

object Join {
	case class Channel(channel: String, key: Option[String] = None)
	object Channel {
		def apply(channel: String, key: String): Channel = apply(channel, Some(key))
	}

	implicit def stringToChannel(channel: String) = Channel(channel)

	def apply(channel: String): Message = Message("JOIN", channel)
	def apply(channel: String, key: String): Message = Message("JOIN", channel, key)

	def apply(channels: Channel*): Message = {
		// sort so any channels without keys end up at the end
		val sorted = channels.sortBy(!_.key.isDefined)
		val chanNames = sorted.map(_.channel) mkString ","
		val keys = sorted.flatMap(_.key) mkString ","
		Message("JOIN", chanNames, keys)
	}
}

object Part {
	def apply(channel: String): Message = Message("PART", channel)
	def apply(channel: String, message: String): Message = Message("PART", channel, message)

	def apply(channels: TraversableOnce[String]): Message = Message("PART", channels mkString ",")
	def apply(channels: TraversableOnce[String], message: String): Message = Message("PART", channels mkString ",", message)
}

// Mode is in TargetedMessages.scala (it is targeted and applies to users as well)