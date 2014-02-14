package cc.baf.irc
package data

case class Prefix(nick: String, user: Option[String], host: Option[String]) {
	def toIrc = s":$nick${user.map(u => s"!$u") getOrElse ""}${host.map(h => s"@$h") getOrElse ""} "
}

case class Message(prefix: Option[Prefix] = None, command: String, params: List[String]) {
	private def renderParams(params: List[String]): List[String] = params match {
		case Nil => Nil
		case last :: Nil if (last.startsWith(":") || last.contains(" ")) => s":$last" :: Nil
		case node :: tail if (node.startsWith(":") || node.contains(" ")) => throw new IllegalArgumentException("middle parameter begins with : or contains a space")
		case node :: tail => node :: renderParams(tail)
	}

	def toIrc = s"${prefix.map(_.toIrc) getOrElse ""}$command${if (!params.isEmpty) renderParams(params).mkString(" ", " ", "") else ""}"
}

object Message {
	def apply(command: String, params: String*): Message = apply(command, params.toList)
	def apply(command: String, params: List[String]): Message = Message(None, command, params)

	def apply(prefix: Option[Prefix], command: String, params: String*): Message = Message(prefix, command, params.toList)

	def apply(prefix: Prefix, command: String, params: String*): Message = apply(prefix, command, params.toList)
	def apply(prefix: Prefix, command: String, params: List[String]): Message = Message(Some(prefix), command, params)
}