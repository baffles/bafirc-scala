package cc.baf.irc
package data.messages

import language.implicitConversions

import cc.baf.irc.data.{ Message, Prefix }
import cc.baf.irc.util.RichSeq._

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

	def unapply(message: Message): Option[(Prefix, MessageTarget, String)] = message match {
		case Message(Some(prefix), `command`, List(MessageTarget(List(destination)), text)) => Some(prefix, destination, text)
		case _ => None
	}
}

object PrivMsg extends TargetedMessage("PRIVMSG")

object Notice extends TargetedMessage("NOTICE")

object Mode {
	sealed trait ModeChange {
		def mode: Char
		def param: Option[String]
	}

	case class Add(mode: Char, param: Option[String] = None) extends ModeChange
	object Add {
		def apply(mode: Char, param: String): Add = Add(mode, Some(param))
	}

	case class Remove(mode: Char, param: Option[String] = None) extends ModeChange
	object Remove {
		def apply(mode: Char, param: String): Remove = Remove(mode, Some(param))
	}

	def apply(target: MessageTarget): Message = Message("MODE", MessageTarget(target))

	def apply(target: MessageTarget, changes: ModeChange*): Message = {
		require(changes.count(_.param.isDefined) <= 3, "No more than three mode changes with parameters allowed.")

		val groupedChanges = changes groupBy {
			case a: Add => "+"
			case r: Remove => "-"
		}

		val modes = groupedChanges map {
			case (change, modes) => s"$change${modes.map(_.mode)}" -> modes.flatMap(_.param).mkString(" ")
		}

		Message("MODE", MessageTarget(target), modes.map(_._1) mkString "", modes.map(_._2) mkString " ")
	}

	def unapply(message: Message): Option[(Prefix, MessageTarget, List[ModeChange])] = message match {
		case Message(Some(prefix), "MODE", args) if args.length >= 2 =>
			val target = MessageTarget.unapply(args.head).get
			val modes = args.drop(1).head.toList
			val modeParams = args.drop(2)

			def expandModes(modifier: Char, modes: List[Char]): List[List[Char]] = modes match {
				case Nil => Nil

				case '+' :: rest => expandModes('+', rest)
				case '-' :: rest => expandModes('-', rest)

				case mode :: rest => List(modifier, mode) :: expandModes(modifier, rest)
			}

			require(modes.head == '+' || modes.head == '-')
			val expandedModes = expandModes(modes.head, modes.tail)

			val paired = expandedModes.zipAll(modeParams.map(Some(_)), "?", None)

			val modeChanges = paired map {
				case (mode, param) =>
					mode match {
						case '+' :: (mode: Char) :: Nil => Add(mode, param)
						case '-' :: (mode: Char) :: Nil => Remove(mode, param)
					}
			}

			Some((prefix, target.single, modeChanges))

		case _ => None
	}
}