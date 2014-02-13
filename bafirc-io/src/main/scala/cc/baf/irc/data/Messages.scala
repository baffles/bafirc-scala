package cc.baf.irc
package data

object Messages {
	object PrivMsg {
		val Command = "PRIVMSG"

		def apply(receiver: String, text: String): Message = apply(receiver :: Nil, text)
		def apply(receivers: List[String], text: String): Message = Message(Command, receivers.mkString(","), text)

		def unapply(message: Message): Option[(Option[Prefix], List[String], String)] = message match {
			case Message(prefix, Command, List(receivers, text)) => Some((prefix, receivers.split(",").toList, text))
			case _ => None
		}
	}
}