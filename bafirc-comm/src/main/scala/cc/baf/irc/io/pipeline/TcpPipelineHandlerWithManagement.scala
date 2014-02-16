package cc.baf.irc
package io.pipeline

import akka.actor._
import akka.io._

import cc.baf.irc.io.Irc

/**
 * Mixin trait for TcpPipelineHandler that will handle management messages as appropriate.
 *
 * @author robertf
 */
trait ManagementHandler extends Actor { tph: TcpPipelineHandler[_, _, _] =>
	protected val handler: ActorRef

	abstract override def receive = super.receive orElse {
		case mm: Irc.ManagementMessage => tph.handler ! TcpPipelineHandler.Management(mm)
	}
}

object TcpPipelineHandlerWithManagement {
	def props[Ctx <: PipelineContext, Cmd, Evt](init: TcpPipelineHandler.Init[Ctx, Cmd, Evt], connection: ActorRef, handlerh: ActorRef) =
		Props { new TcpPipelineHandler(init, connection, handlerh) with ManagementHandler { val handler = handlerh } }
}