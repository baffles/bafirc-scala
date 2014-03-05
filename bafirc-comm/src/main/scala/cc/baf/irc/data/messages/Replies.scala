package cc.baf.irc
package data.messages

import scala.util.Try

import cc.baf.irc.data.{ Message, Prefix }
import cc.baf.irc.util.AsMatchers.AsInt

object Reply {
	val Numeric = AsInt

	abstract class BasicMessageMatcher(numeric: Int) extends MessageMatcher[(Prefix, String)] {
		def matcher = {
			case Message(Some(prefix), Numeric(`numeric`), List(_, message)) => (prefix, message)
		}
	}

	//object Welcome extends BasicMessageMatcher(1)
	object Welcome extends MessageMatcher[(Prefix, String, String)] {
		def matcher = {
			case Message(Some(prefix), Numeric(1), List(nick, message)) => (prefix, nick, message)
		}
	}

	object YourHost extends BasicMessageMatcher(2)
	object Created extends BasicMessageMatcher(3)
	object MyInfo extends BasicMessageMatcher(4)
	object Bounce extends BasicMessageMatcher(5)

	object Away extends MessageMatcher[(Prefix, String, String)] {
		def matcher = {
			case Message(Some(prefix), Numeric(301), List(_, nick, message)) => (prefix, nick, message)
		}
	}

	object UnAway extends BasicMessageMatcher(305)
	object NowAway extends BasicMessageMatcher(306)

	case class UserHostInfo(nickname: String, hostname: String, isOperator: Boolean, isAway: Boolean)
	object UserHost extends MessageMatcher[(Prefix, List[UserHostInfo])] {
		private val UserHostReply = """([^*=]+)(\*?)=([+\-])(.*?)""".r

		def matcher = {
			case Message(Some(prefix), Numeric(302), List(_, repliesStr)) =>
				val replies = (repliesStr split ' ') map {
					case UserHostReply(nickname, isOper, isAway, hostname) =>
						UserHostInfo(nickname, hostname, isOper == "*", isAway == "+")
				}
				prefix -> replies.toList
		}
	}

	object IsOn extends MessageMatcher[(Prefix, List[String])] {
		def matcher = {
			case Message(Some(prefix), Numeric(303), List(_, repliesStr)) =>
				val nicks = (repliesStr split ' ')
				prefix -> nicks.toList
		}
	}

	case class WhoisUserInfo(nickname: String, username: String, hostname: String, realName: String)
	object WhoisUser extends MessageMatcher[(Prefix, WhoisUserInfo)] {
		def matcher = {
			case Message(Some(prefix), Numeric(311), List(_, nick, user, host, "*", real)) =>
				prefix -> WhoisUserInfo(nick, user, host, real)
		}
	}

	case class WhoisServerInfo(server: String, serverInfo: String)
	object WhoisServer extends MessageMatcher[(Prefix, WhoisServerInfo)] {
		def matcher = {
			case Message(Some(prefix), Numeric(312), List(_, server, info)) =>
				prefix -> WhoisServerInfo(server, info)
		}
	}

	object WhoisOperator extends BasicMessageMatcher(313)

	case class WhoisIdleInfo(nickname: String, secondsIdle: Int)
	object WhoisIdle extends MessageMatcher[(Prefix, WhoisIdleInfo)] {
		def matcher = {
			case Message(Some(prefix), Numeric(317), List(_, nick, AsInt(secs), _)) =>
				prefix -> WhoisIdleInfo(nick, secs)
		}
	}

	object EndOfWhois extends BasicMessageMatcher(318)

	case class WhoisChannelInfo(nickname: String, channels: List[WhoisChannelInfoChannel])
	case class WhoisChannelInfoChannel(channel: String, modes: List[Char])
	object Whoishannels extends MessageMatcher[(Prefix, WhoisChannelInfo)] {
		private val Channel = """([^#+!&]*)(.*)""".r

		def matcher = {
			case Message(Some(prefix), Numeric(319), List(_, nick, channelsStr)) =>
				val channels = (channelsStr split ' ') map {
					case Channel(modes, chan) => WhoisChannelInfoChannel(chan, modes.toList)
				}
				prefix -> WhoisChannelInfo(nick, channels.toList)
		}
	}

	object WhoWasUser extends MessageMatcher[(Prefix, WhoisUserInfo)] {
		def matcher = {
			case Message(Some(prefix), Numeric(314), List(_, nick, user, host, "*", real)) =>
				prefix -> WhoisUserInfo(nick, user, host, real)
		}
	}

	object EndOfWhoWas extends BasicMessageMatcher(369)

	case class ChannelListInfo(channel: String, visibleUsers: Int, topic: String)
	object ChannelList extends MessageMatcher[(Prefix, ChannelListInfo)] {
		def matcher = {
			case Message(Some(prefix), Numeric(322), List(_, channel, AsInt(numVisible), topic)) =>
				prefix -> ChannelListInfo(channel, numVisible, topic)
		}
	}

	object ChannelListEnd extends BasicMessageMatcher(323)

	object NicknameInUse extends MessageMatcher[(Prefix, String, String)] {
		def matcher = {
			case Message(Some(prefix), Numeric(433), List("*", nick, message)) => (prefix, nick, message)
		}
	}

	// /** Info about the server/network */
	// case class MyInfo(serverName: String, version: String, userModes: List[Char], channelModes: List[Char]) extends Reply(4)
	//  object MyInfo extends MessageMatcher {
	// 	lazy val InfoMessage = """([a-zA-Z0-9\-\.]+) ([^ ]+) ([^ ]+) ([^ ]+).*""".r
	// 	def matcher = {
	// 		case Message(_, Numeric(4), List(InfoMessage(serverName, version, userModes, channelModes))) => MyInfo(serverName, version, userModes.toList, channelModes.toList)
	// 	}
	// }

	// /** Connection refused, essentially */
	// case class Bounce(message: String) extends Reply(5)
	// object Bounce extends BasicMessageMatcher(5)

	// // 302: RPL_USERHOST
	// // 303: RPL_ISON

	// case class Away(nick: String, message: String) extends Reply(301)
	// case object UnAway extends Reply(305)
	// case object NowAway extends Reply(306)
	// object AwayMatchers extends MessageMatcher {
	// 	def matcher = {
	// 		case Message(_, Numeric(301), List(nick, message)) => Away(nick, message)
	// 		case Message(_, Numeric(305), _) => UnAway
	// 		case Message(_, Numeric(306), _) => NowAway
	// 	}
	// }

	// case class Unknown(numeric: Int, params: List[String]) extends Reply(numeric)
	// object Unknown extends MessageMatcher {
	// 	def matcher = {
	// 		case Message(_, Numeric(numeric), params) => Unknown(numeric, params)
	// 	}
	// }

	// private val matchers = List(
	// 	Welcome, YourHost, Created, MyInfo, Bounce,
	// 	AwayMatchers,
	// 	NicknameInUse,
	// 	Unknown)
	// private lazy val unapplicator = matchers.map(_.matcher).reduceLeft { _ orElse _ }

	// def unapply(message: Message): Option[Reply] = unapplicator lift message
}