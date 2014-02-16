package cc.baf.irc
package io

import scala.concurrent.duration.{ Duration, FiniteDuration }

import akka.actor._
import akka.event.LoggingAdapter
import akka.io.{ IO, _ }
import akka.util._

import cc.baf.irc.data.Message
import cc.baf.irc.io.pipeline._

/**
 * The main glue that provides for IRC connections.
 * TODO: SSL support
 *
 * @author robertf
 */
private[io] class IrcConnection(commander: ActorRef, connect: Irc.Connect, settings: IrcConnectionSettings) extends Actor with ActorLogging { actor =>
	import IrcConnection._
	import context.system
	import connect._
	import settings._

	log debug s"Attempting connection to $remoteAddress"

	IO(Tcp) ! Tcp.Connect(remoteAddress, localAddress, options)

	context setReceiveTimeout connectTimeout

	def receive: Receive = {
		case connected: Tcp.Connected =>
			log debug s"Connected to ${connected.remoteAddress}"
			context setReceiveTimeout Duration.Undefined

			val pipeline = context.actorOf(TcpPipelineHandlerWithManagement.props(pipelineInit, sender, self))

			sender ! Tcp.Register(pipeline)
			context watch sender
			context watch pipeline
			commander ! connected

			context become running(pipeline)
			scheduleSendQueueTick
	}

	def running(pipeline: ActorRef): Receive = {
		case pipelineInit.Event(msg) => commander ! msg
		case TcpPipelineHandler.Management(mgm) => commander ! mgm

		case msg: Message => pipeline ! pipelineInit.Command(msg)
		case mgm: Irc.ManagementMessage => pipeline ! TcpPipelineHandler.Management(mgm)
		case FirePollQueue => scheduleSendQueueTick(); self ! Irc.PollQueue
		
		case Terminated(`commander`) | Terminated(`pipeline`) => context stop self
	}
	
	private case object FirePollQueue
	private def scheduleSendQueueTick() = system.scheduler.scheduleOnce(throttlePenalty.asInstanceOf[FiniteDuration], self, FirePollQueue)(context.dispatcher)

	private val pipelineInit = new TcpPipelineHandler.Init[Context, Message, Message](
		new MessageThrottling(throttlingEnabled, throttlePenalty.toMillis, throttleWindow.toMillis)(throttlingPriorityProvider.MessageOrdering) >>
			new IrcMessageStage >>
			new MessageFraming(maxMessageSize, charset) >>
			new TcpReadWriteAdapter >>
			new TickGenerator(throttlePenalty.asInstanceOf[FiniteDuration])
	) {
		def makeContext(ctx: ActorContext) = new Context {
			def getLogger = log
			def getContext = ctx
		}
	}
}

private[io] object IrcConnection {
	trait Context extends PipelineContext with HasLogging with HasActorContext

	def props(commander: ActorRef, connect: Irc.Connect, settings: IrcConnectionSettings) = Props { new IrcConnection(commander, connect, settings) }
}