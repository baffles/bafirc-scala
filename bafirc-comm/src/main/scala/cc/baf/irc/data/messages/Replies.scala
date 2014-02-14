package cc.baf.irc
package data.messages

import util.Try

import cc.baf.irc.data.Message

sealed abstract class Reply(numeric: Int)
object Reply {
	private trait ReplyMatcher {
		def matcher: PartialFunction[Message, Reply]

		object Numeric {
			def unapply(str: String): Option[Int] = Try(str.toInt).toOption
		}
	}

	private abstract class BasicReplyMatcher(numeric: Int) extends ReplyMatcher {
		def apply(str: String): Reply

		def matcher = {
			case Message(_, Numeric(`numeric`), List(message)) => apply(message)
		}
	}

	/** First welcome/registration message */
	case class Welcome(nick: String, user: String, host: String) extends Reply(1)
	private object Welcome extends ReplyMatcher {
		lazy val WelcomeMessage = """Welcome to the Internet Relay Network ([a-zA-Z0-9\-\[\]`\^{}]+)!(.+?)@([a-zA-Z0-9\-\.]+)""".r
		def matcher = {
			case Message(_, Numeric(1), List(WelcomeMessage(nick, user, host))) => Welcome(nick, user, host)
		}
	}

	/** Host info about the server */
	case class YourHost(message: String) extends Reply(2)
	private object YourHost extends BasicReplyMatcher(2)

	/** Server creation info */
	case class Created(message: String) extends Reply(3)
	private object Created extends BasicReplyMatcher(3)

	/** Info about the server/network */
	case class MyInfo(serverName: String, version: String, userModes: List[Char], channelModes: List[Char]) extends Reply(4)
	private object MyInfo extends ReplyMatcher {
		lazy val InfoMessage = """([a-zA-Z0-9\-\.]+) ([^ ]+) ([^ ]+) ([^ ]+).*""".r
		def matcher = {
			case Message(_, Numeric(4), List(InfoMessage(serverName, version, userModes, channelModes))) => MyInfo(serverName, version, userModes.toList, channelModes.toList)
		}
	}

	/** Connection refused, essentially */
	case class Bounce(message: String) extends Reply(5)
	private object Bounce extends BasicReplyMatcher(5)

	// 302: RPL_USERHOST
	// 303: RPL_ISON

	case class Away(nick: String, message: String) extends Reply(301)
	case object UnAway extends Reply(305)
	case object NowAway extends Reply(306)
	private object AwayMatchers extends ReplyMatcher {
		def matcher = {
			case Message(_, Numeric(301), List(nick, message)) => Away(nick, message)
			case Message(_, Numeric(305), _) => UnAway
			case Message(_, Numeric(306), _) => NowAway
		}
	}

	case class Unknown(numeric: Int, params: List[String]) extends Reply(numeric)
	private object Unknown extends ReplyMatcher {
		def matcher = {
			case Message(_, Numeric(numeric), params) => Unknown(numeric, params)
		}
	}

	private val matchers = List(
		Welcome, YourHost, Created, MyInfo, Bounce,
		AwayMatchers,
		Unknown)
	private lazy val unapplicator = matchers.map(_.matcher).reduceLeft { _ orElse _ }

	def unapply(message: Message): Option[Reply] = unapplicator lift message
}