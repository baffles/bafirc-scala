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

	def apply(channels: Channel*): Message = apply(channels.toList)
	def apply(channels: List[Channel]) = {
		// sort so any channels without keys end up at the end
		val sorted = channels.sortBy(!_.key.isDefined)
		val chanNames = sorted.map(_.channel) mkString ","
		val keys = sorted.flatMap(_.key) mkString ","
		Message("JOIN", chanNames, keys)
	}
}

object Part extends MessageMatcher[(Prefix, String, Option[String])] {
	def apply(channel: String): Message = Message("PART", channel)
	def apply(channel: String, message: String): Message = Message("PART", channel, message)

	def apply(channels: TraversableOnce[String]): Message = Message("PART", channels mkString ",")
	def apply(channels: TraversableOnce[String], message: String): Message = Message("PART", channels mkString ",", message)

	def matcher = {
		case Message(Some(prefix), "PART", List(channel)) => (prefix, channel, None)
		case Message(Some(prefix), "PART", List(channel, message)) => (prefix, channel, Some(message))
	}
}

// Mode is in TargetedMessages.scala (it is targeted and applies to users as well)

object Topic extends MessageMatcher[(Prefix, String, Option[String])] {
	// query for topic
	def apply(channel: String): Message = Message("TOPIC", channel)

	// set or clear topic
	def apply(channel: String, topic: String): Message = apply(channel, Some(topic))
	def apply(channel: String, topic: Option[String]): Message = Message("TOPIC", channel, topic getOrElse "")

	def matcher = {
		case Message(Some(prefix), "TOPIC", List(channel, topic)) => (prefix, channel, if (!topic.isEmpty) Some(topic) else None)
	}
}

object Names {
	def apply(): Message = Message("NAMES")
	def apply(channels: String*): Message = apply(channels)
	def apply(channels: TraversableOnce[String]): Message = Message("NAMES", channels mkString ",")
	def apply(channels: TraversableOnce[String], target: String): Message = Message("NAMES", channels mkString ",", target)
}

object ChannelList {
	def apply(): Message = Message("LIST")
	def apply(channels: String*): Message = apply(channels)
	def apply(channels: TraversableOnce[String]): Message = Message("LIST", channels mkString ",")
	def apply(channels: TraversableOnce[String], target: String): Message = Message("LIST", channels mkString ",", target)
}

object Invite extends MessageMatcher[(Prefix, String)] {
	def apply(nickname: String, channel: String) = Message("INVITE", nickname, channel)

	def matcher = {
		case Message(Some(prefix), "INVITE", List(channel)) => prefix -> channel
	}
}

object Kick extends MessageMatcher[(Prefix, String, String, Option[String])] {
	def apply(channel: String, nickname: String): Message = apply(Seq(channel), Seq(nickname))
	def apply(channel: String, nickname: String, comment: String): Message = apply(Seq(channel), Seq(nickname), comment)

	def apply(channels: TraversableOnce[String], nickname: String): Message = apply(channels, Seq(nickname))
	def apply(channels: TraversableOnce[String], nickname: String, comment: String): Message = apply(channels, Seq(nickname), comment)

	def apply(channel: String, nicknames: TraversableOnce[String]): Message = apply(Seq(channel), nicknames)
	def apply(channel: String, nicknames: TraversableOnce[String], comment: String): Message = apply(Seq(channel), nicknames, comment)

	def apply(channels: TraversableOnce[String], nicknames: TraversableOnce[String]): Message = Message("KICK", channels mkString ",", nicknames mkString ",")
	def apply(channels: TraversableOnce[String], nicknames: TraversableOnce[String], comment: String): Message = Message("KICK", channels mkString ",", nicknames mkString ",", comment)

	def matcher = {
		case Message(Some(prefix), "KICK", List(channel, nickname)) => (prefix, channel, nickname, None)
		case Message(Some(prefix), "KICK", List(channel, nickname, comment)) => (prefix, channel, nickname, Some(comment))
	}
}