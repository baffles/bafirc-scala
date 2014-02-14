package cc.baf.irc
package io

import scala.concurrent.duration.Duration

import akka.actor.{ Actor, ActorContext, ActorRef, ActorLogging, Props, ReceiveTimeout, SupervisorStrategy }
import akka.event.LoggingAdapter
import akka.io._
import akka.util._

import cc.baf.irc.data.Message
import cc.baf.irc.io.pipeline._

/**
 * The main glue that provides for IRC connections.
 * TODO: SSL support
 *
 * @author robertf
 */
private[io] class IrcConnection(commander: ActorRef, connect: Irc.Connect) extends Actor with ActorLogging { actor =>
	import IrcConnection._
	import context.system
	import connect._
	import pipelineInit.{ Command, Event }

	log debug s"Attempting connection to $remoteAddress"

	IO(Tcp) ! Tcp.Connect(remoteAddress, localAddress, options)

	//context setReceiveTimeout settings.connectTimeout

	def receive: Receive = {
		case connected: Tcp.Connected =>
			log debug s"Connected to ${connected.remoteAddress}"
			//context setReceiveTimeout Duration.Undefined

			val pipeline = context.actorOf(TcpPipelineHandler.props(pipelineInit, sender, self))

			sender ! Tcp.Register(pipeline)
			context watch sender
			context watch pipeline
			commander ! connected

			context become running(sender, pipeline)
	}

	def running(pipeline: ActorRef): Receive = {
		case msg: Message => pipeline ! Command(msg)
		case Event(msg: Message) => commander ! msg
		
		case Terminated(`commander`) | Terminated(`pipeline`) => context stop self
	}

	private val pipelineInit = new TcpPipelineHandler.Init[Context, Message, Message](
		new IrcMessageStage >>
			new MessageFraming >>
			new TcpReadWriteAdapter
	) {
		def makeContext(ctx: ActorContext) = new Context {
			def getLogger = log
			def getContext = ctx
		}
	}
}

private[io] object IrcConnection {
	trait Context extends PipelineContext with HasLogging with HasActorContext

	def props(commander: ActorRef, connect: Irc.Connect) = Props { new IrcConnection(commander, connect) }
}