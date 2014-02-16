package cc.baf.irc
package io

import java.net.InetSocketAddress

import scala.collection.immutable
import scala.concurrent.duration.Duration

import akka.actor._
import akka.io._

private[io] class IrcManager(settings: IrcExt#Settings) extends Actor with ActorLogging {
	import settings._

	val connectionCounter = Iterator from 0

	def receive = {
		case connect: Irc.Connect =>
			context.actorOf(
				IrcConnection.props(sender, connect, IrcConnectionSettings(connect.settings)) withDispatcher settings.ConnectionDispatcher,
				name = s"irc-connection-${connectionCounter.next}"
			)
	}
}