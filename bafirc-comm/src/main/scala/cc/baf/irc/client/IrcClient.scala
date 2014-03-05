package cc.baf.irc
package client

import java.net.InetSocketAddress

import collection.mutable.Queue

import akka.actor._
import akka.io.IO

import cc.baf.irc.data._
import cc.baf.irc.data.messages._
import cc.baf.irc.io._

case class IrcServer(host: String, port: Int = 6667, sslEnabled: Boolean = false, password: Option[String] = None) {
	lazy val connect = Irc.Connect(host, port, sslEnabled)
}

case class IrcClientSettings(
	serverList: List[IrcServer],
	nicknameProvider: NicknameProvider,
	username: String,
	realname: String
)

object IrcClient {
	case class Registered(server: IrcServer, nickname: String)
}

/**
 * IrcClient provides a wrapper around IRC connections to handle some of the basic, mundane tasks, such as connection
 * registration, ping/pong replies, and reconnecting.
 *
 * @author robertf
 */
class IrcClient(settings: IrcClientSettings, handler: ActorRef) extends Actor with ActorLogging with Stash {
	import context.system
	import settings._
	import IrcClient._

	require(!serverList.isEmpty, "settings.serverList must not be empty")
	//TODO: validate nicknames/username/realname?

	private val serverTracker = new ServerTracker(serverList)

	connect()

	def receive = PartialFunction.empty

	private def connect(newServer: Boolean = false) {
		if (newServer)
			serverTracker.newServer

		val server = serverTracker.currentServer

		context become {
			case connected: Irc.Connected =>
				handler ! connected
				register(server, sender)

			// stash everything else for handling later
			// TODO: look for failed connection attempts/etc
			case _ => stash
		}

		IO(Irc) ! server.connect
	}

	private def register(server: IrcServer, irc: ActorRef) {
		context become {
			case err @ Reply.NicknameInUse(_, nick, msg) =>
				handler ! err
				irc ! Nick(nicknameProvider.nextNick)

			case welcome @ Reply.Welcome(_, myNick, msg) =>
				handler ! welcome
				handler ! Registered(server, myNick)
				handleSession(server, irc)

			// relay everything from irc connection to handler
			case msg if sender == irc => handler ! msg

			// stash everything else for later
			case _ => stash
		}

		for (pw <- serverTracker.currentServer.password)
			irc ! Password(pw)
		irc ! Nick(nicknameProvider.nextNick)
		irc ! User(username, 0, realname)
	}

	private def handleSession(server: IrcServer, irc: ActorRef) {
		context become {
			case ping @ Ping(s1, s2) =>
				handler ! ping
				irc ! Pong(s1, s2)

			// relay everything from irc connection to handler
			case msg if sender == irc => handler ! msg

			// relay everything from anyone else to the irc connection
			case msg => irc ! msg
		}

		unstashAll
	}
}