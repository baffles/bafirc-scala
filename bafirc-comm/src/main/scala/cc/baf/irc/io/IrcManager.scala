package cc.baf.irc
package io

import java.net.InetSocketAddress

import scala.collection.immutable
import scala.concurrent.duration.Duration

import akka.actor._
import akka.io._

private[io] class IrcManager() extends Actor with ActorLogging {
	val connectionCounter = Iterator from 0

	def receive = {
		case connect: Irc.Connect => context.actorOf(IrcConnection.props(sender, connect), name = s"irc-connection-${connectionCounter.next}")
	}
}