package cc.baf.irc
package data.messages

import language.implicitConversions

import cc.baf.irc.data.{ Message, Prefix }

sealed trait MessageTarget { def target: String }
object MessageTarget {
	case class Channel(channelName: String) extends MessageTarget { def target = channelName }
	case class User(nickname: String) extends MessageTarget { def target = nickname }

	implicit def stringToMessageTarget(target: String) = unapply(target).get

	private def isChannel(target: String) = (target startsWith "#") || (target startsWith "+") || (target startsWith "!") || (target startsWith "&")
	
	def apply(targets: MessageTarget*): String = apply(targets)
	def apply(targets: TraversableOnce[MessageTarget]): String = targets.map(_.target).mkString(",")
	
	def unapply(targetString: String) = Some {
		targetString.split(",").toList map {
			case chan if isChannel(chan) => Channel(chan)
			case nick => User(nick)
		}
	}
}

abstract class TargetedMessage(command: String) {
	def apply(receiver: MessageTarget, text: String): Message = apply(receiver :: Nil, text)
	def apply(receivers: TraversableOnce[MessageTarget], text: String): Message = Message(command, MessageTarget(receivers), text)

	def unapply(message: Message): Option[(Option[Prefix], MessageTarget, String)] = message match {
		case Message(prefix, `command`, List(MessageTarget(List(destination)), text)) => Some(prefix, destination, text)
		case _ => None
	}
}

object PrivMsg extends TargetedMessage("PRIVMSG")

object Notice extends TargetedMessage("NOTICE")