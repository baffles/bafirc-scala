package cc.baf.irc
package data.messages

import cc.baf.irc.data.{ Message, Prefix }

object Password {
	def apply(password: String) = Message("PASSWORD", password)
}

object Nick {
	def apply(nickname: String) = Message("NICK", nickname)

	def unapply(message: Message): Option[(Prefix, String)] = message match {
		case Message(Some(prefix), "NICK", List(newNick)) => Some(prefix, newNick)
		case _ => None
	}
}

object User {
	def apply(username: String, mode: String, realname: String) = Message("USER", username, mode, "*", realname)
}

object Oper {
	def apply(name: String, password: String) = Message("OPER", name, password)
}

object Quit {
	def apply() = Message("QUIT")
	def apply(message: String) = Message("QUIT", message)
}

object Motd {
	def apply() = Message("MOTD")
	def apply(target: String) = Message("MOTD", target)
}