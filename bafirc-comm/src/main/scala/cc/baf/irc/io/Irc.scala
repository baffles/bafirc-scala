package cc.baf.irc
package io

import java.net.InetSocketAddress

import scala.collection.immutable
import scala.concurrent.duration.Duration

import akka.actor._
import akka.io.{ IO, _ }

case class IrcClientSettings(connectTimeout: Duration)

object Irc extends ExtensionKey[IrcExt] {

	case class Connect(remoteAddress: InetSocketAddress, sslEnabled: Boolean, localAddress: Option[InetSocketAddress], options: immutable.Traversable[Inet.SocketOption], settings: Option[IrcClientSettings])
	object Connect {
		def apply(
				host: String,
				port: Int = 6667,
				sslEnabled: Boolean = false,
				localAddress: Option[InetSocketAddress] = None,
				options: immutable.Traversable[Inet.SocketOption] = Nil,
				settings: Option[IrcClientSettings] = None
			): Connect = {

			apply(new InetSocketAddress(host, port), sslEnabled, localAddress, options, settings)
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
}

class IrcExt(system: ExtendedActorSystem) extends IO.Extension {

	val manager = system.actorOf(
		props = Props(new IrcManager),
		name = "IO-IRC")
}