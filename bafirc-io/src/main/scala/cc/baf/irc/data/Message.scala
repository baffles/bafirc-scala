package cc.baf.irc
package data

case class Prefix(nick: String, user: Option[String], host: Option[String])
case class Message(prefix: Option[Prefix] = None, command: String, params: List[String])

object Message {
	def apply(command: String, params: String*): Message = apply(command, params.toList)
	def apply(command: String, params: List[String]): Message = Message(None, command, params)

	def apply(prefix: Option[Prefix], command: String, params: String*): Message = Message(prefix, command, params.toList)

	def apply(prefix: Prefix, command: String, params: String*): Message = apply(prefix, command, params.toList)
	def apply(prefix: Prefix, command: String, params: List[String]): Message = Message(Some(prefix), command, params)
}