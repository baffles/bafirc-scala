package cc.baf.irc
package parser

import org.parboiled.scala._
import java.lang.String
import org.parboiled.errors._

import scala.util.Try

import cc.baf.irc.data.{ Prefix => DPrefix, Message => DMessage}

/**
 * Parser for IRC messages, as defined by RFC1459. Not thread safe (use one instance per thread).
 *
 * @author robertf
 */
class IrcParser extends Parser {
	def IrcMessage = rule {
		optional(":" ~ Prefix ~ Space) ~ Command ~> identity ~ Params ~> identity ~ optional(CrLf) ~ EOI ~~> { (prefix: Option[DPrefix], command: String, params: List[String], _: String) =>
			DMessage(prefix, command, params)
		}
	}

	def Prefix: Rule1[DPrefix] = rule { NickPrefix | ServerNamePrefix }
	def ServerNamePrefix = rule { ServerName ~> { name: String => DPrefix(name, None, None) } }
	def NickPrefix = rule { Nick ~> identity ~ optional("!" ~ User ~> identity) ~ optional("@" ~ Host ~> identity) ~~> DPrefix }

	def Command = rule { oneOrMore(Letter) | nTimes(3, Number) }
	def Params = rule {
		Space ~ zeroOrMore(MiddleParam ~> identity, separator = Space) ~ optional(Space ~ ":" ~ TrailingParam ~> identity) ~~> { (middles: List[String], trailing: Option[String]) =>
			trailing match {
				case Some(trailing) => middles ++ (trailing :: Nil)
				case None => middles
			}
		}
	}
	def MiddleParam = rule { noneOf(":\0\r\n ") ~ zeroOrMore(noneOf("\0\r\n ")) }
	def TrailingParam = rule { zeroOrMore(noneOf("\0\r\n")) }

	def Target = rule { oneOrMore(To, separator = ",") }

	def To = rule { Channel | (User ~ "@" ~ ServerName) | Nick | Mask }
	def Channel = rule { anyOf("#&") ~ ChString }
	def ServerName = rule { Host }

	def Nick = rule { Letter ~ zeroOrMore(Letter | Number | Special) }
	def User = rule { oneOrMore(noneOf(" @\0\r\n")) }
	def Host = rule { (Letter | Number | "-") ~ zeroOrMore(Letter | Number | "-" | ".") }
	def ChString = rule { noneOf(" \b\0\r\n,") }
	def Mask = rule { ("#" | "$") ~ ChString }

	def Letter = rule { "a" - "z" | "A" - "Z" }
	def Number = rule { "0" - "9" }
	def Special = rule { "-" | "[" | "]" | "\\" | "`" | "^" | "{" | "}" }

	def Space: Rule0 = rule { oneOrMore(" ") }
	def CrLf: Rule0 = rule { zeroOrMore(anyOf("\r\n")) }

	private lazy val parser = ReportingParseRunner(IrcMessage)

	def parseMessage(message: String): DMessage = {
		val result = parser.run(message)
		result.result match {
			case Some(msg) => msg
			case None => throw new ParsingException(s"Invalid IRC Message:\n${ErrorUtils.printParseErrors(result)}")
		}
	}

	def tryParseMessage(message: String) = Try { parseMessage(message) }
}

object test {
	def main(args: Array[String]) {
		val parser = new IrcParser()

		println(parser.tryParseMessage(":BAF!ferrisr@vort PRIVMSG Sevalecan :you suck"))
		println(parser.tryParseMessage("!BROKEN MESSAGE Sevalecan :you suck"))
		println(parser.tryParseMessage(":!BROKEN PREFIX Sevalecan :you suck"))
		println(parser.tryParseMessage(":BAF!ferrisr@vort PRIVMSG"))

		println
		println
		println
		import cc.baf.irc.data.Messages

		for {
			parsed <- parser.tryParseMessage(":BAF!ferrisr@vort PRIVMSG Sevalecan :you suck")
		} {
			parsed match {
				case Messages.PrivMsg(Some(prefix), target :: Nil, message) =>
					println(s"${prefix.nick} tells $target: $message")
			}
		}
	}
}