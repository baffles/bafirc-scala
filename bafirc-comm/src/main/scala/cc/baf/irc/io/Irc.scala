package cc.baf.irc
package io

import java.net.InetSocketAddress

import scala.collection.immutable
import scala.concurrent.duration.FiniteDuration

import akka.actor._
import akka.io.{ IO, _ }
import com.typesafe.config.Config

import cc.baf.irc.util.MessagePriorityProvider

object Irc extends ExtensionKey[IrcExt] {

	case class Connect(remoteAddress: InetSocketAddress, sslEnabled: Boolean, throttlingEnabled: Boolean, throttlingPriorityProvider: MessagePriorityProvider, localAddress: Option[InetSocketAddress], options: immutable.Traversable[Inet.SocketOption], settings: Option[IrcConnectionSettings])
	object Connect {
		def apply(
				host: String,
				port: Int = 6667,
				sslEnabled: Boolean = false,
				throttlingEnabled: Boolean = true,
				throttlingPriorityProvider: MessagePriorityProvider = MessagePriorityProvider.DefaultProvider,
				localAddress: Option[InetSocketAddress] = None,
				options: immutable.Traversable[Inet.SocketOption] = Nil,
				settings: Option[IrcConnectionSettings] = None
			): Connect = {

			apply(new InetSocketAddress(host, port), sslEnabled, throttlingEnabled, throttlingPriorityProvider, localAddress, options, settings)
		}
	}

	type Connected = Tcp.Connected
	val Connected = Tcp.Connected
	type ConnectionClosed = Tcp.ConnectionClosed

	val Closed = Tcp.Closed
	val Aborted = Tcp.Aborted
	val ConfirmedClose = Tcp.ConfirmedClose
	val PeerClosed = Tcp.PeerClosed
	type ErrorClosed = Tcp.ErrorClosed
	val ErrorClosed = Tcp.ErrorClosed

	sealed trait ManagementMessage

	case class QueueStats(queued: Int, eta: FiniteDuration) extends ManagementMessage
	case object FlushQueue extends ManagementMessage
	private[io] case object PollQueue extends ManagementMessage // used internally
}

class IrcExt(system: ExtendedActorSystem) extends IO.Extension {

	val Settings = new Settings(system.settings.config getConfig "baf.irc.io")
	class Settings private[IrcExt](config: Config) {
		val ManagerDispatcher = config getString "manager-dispatcher"
		val ConnectionDispatcher = config getString "connection-dispatcher"
	}

	val manager = system.actorOf(
		props = Props(new IrcManager(Settings)) withDispatcher Settings.ManagerDispatcher,
		name = "IO-IRC")
}