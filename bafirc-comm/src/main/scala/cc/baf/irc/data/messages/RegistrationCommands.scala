package cc.baf.irc
package data.messages

import cc.baf.irc.data.{ Message, Prefix }

object Password {
	def apply(password: String) = Message("PASSWORD", password)
}

object Nick extends MessageMatcher[(Prefix, String)] {
	def apply(nickname: String) = Message("NICK", nickname)

	def matcher = {
		case Message(Some(prefix), "NICK", List(newNick)) => prefix -> newNick
	}
}

object User {
	def apply(username: String, mode: Int, realname: String) = Message("USER", username, mode.toString, "*", realname)
}

object Oper {
	def apply(name: String, password: String) = Message("OPER", name, password)
}

object Quit {
	def apply() = Message("QUIT")
	def apply(message: String) = Message("QUIT", message)

	def unapply(message: Message): Option[(Prefix, Option[String])] = message match {
		case Message(Some(prefix), "QUIT", List()) => Some(prefix, None)
		case Message(Some(prefix), "QUIT", List(message)) => Some(prefix, Some(message))
		case _ => None
	}
}

object SQuit {
	def apply(server: String) = Message("SQUIT", server)
	def apply(server: String, message: String) = Message("SQUIT", server, message)
}




object Motd {
	def apply() = Message("MOTD")
	def apply(target: String) = Message("MOTD", target)
}


object Ping extends MessageMatcher[(String, Option[String])] {
	def apply(server: String): Message = Message("PING", server)
	def apply(server1: String, server2: String): Message = Message("PING", server1, server2)

	def apply(server1: String, server2: Option[String]): Message = server2 match {
		case Some(server2) => apply(server1, server2)
		case None => apply(server1)
	}

	def matcher = {
		case Message(_, "PING", List(server)) => (server, None)
		case Message(_, "PING", List(server1, server2)) => (server1, Some(server2))
	}
}

object Pong {
	def apply(server: String): Message = Message("PONG", server)
	def apply(server1: String, server2: String): Message = Message("PONG", server1, server2)

	def apply(server1: String, server2: Option[String]): Message = server2 match {
		case Some(server2) => apply(server1, server2)
		case None => apply(server1)
	}
}