package cc.baf.irc
package io.pipeline

import java.nio.charset.Charset

import scala.annotation.tailrec
import scala.collection.mutable.PriorityQueue
import scala.concurrent.duration._
import scala.math.Ordering

import akka.io._
import akka.util._

import cc.baf.irc.data.Message
import cc.baf.irc.io.Irc
import cc.baf.irc.util.MessagePriorityProvider

/**
 * Provides throttling/flood control, as described in RFC 1459, section 8.10.
 *
 * Management messages are used, as defined in io.Irc, to both notify upstream of the send queue status, and also to
 * allow consumers to forcefully flush the queue. Messages are ordered in priority by the implicit Ordering[Message].
 *
 * @param enabled whether throttling is enabled
 * @param penalty the penalty per message, in ms
 * @param window the window for throttling, in ms
 * @param priority ordering for message prioritization
 *
 * @author robertf
 */
private[io] class MessageThrottling(
		enabled: Boolean,
		penalty: Long,
		window: Long
	)(implicit priority: Ordering[Message]) extends SymmetricPipelineStage[HasActorContext, Message, Message] {

	def apply(context: HasActorContext) = new SymmetricPipePair[Message, Message] {
		private var messageTimer = 0L
		private var lastActualSend = 0L
		private val sendQueue = PriorityQueue.empty[Message]

		private def sendStats() {
			val eta = penalty * sendQueue.length

			val stats = Irc.QueueStats(sendQueue.length, eta.millis)
			context.getContext.self ! stats
		}

		private def processQueue() = {
			val canSend = (System.currentTimeMillis - lastActualSend) / penalty
			if (canSend >= 1) {
				// send what we can
				val toSend = (1 to canSend.toInt).iterator.takeWhile(_ => !sendQueue.isEmpty).map(_ => sendQueue.dequeue).toList
				doActualSend(toSend)
			} else {
				// we can't send anything. no-op
				Nil
			}
		}

		private def doActualSend(message: Message) = {
			lastActualSend = System.currentTimeMillis
			context singleCommand message
		}

		private def doActualSend(messages: List[Message]) = {
			lastActualSend = System.currentTimeMillis
			if (messages.length == 1)
				context singleCommand messages.head
			else
				messages.map(Right(_))
		}

		def commandPipeline = { message: Message =>
			val shouldQueue = if(enabled) {
				val curTime = System.currentTimeMillis

				messageTimer = math.max(messageTimer, curTime)
				messageTimer += penalty

				!sendQueue.isEmpty || messageTimer >= curTime + window
			} else false

			if (shouldQueue) {
				// queue message and no-op
				sendQueue += message
				Nil
			} else {
				// otherwise, send it immediately
				doActualSend(message)
			}
		}

		def eventPipeline = { message: Message =>
			// no throttling on incoming messages
			context singleEvent message
		}

		override def managementPort = super.managementPort orElse {
			// on each tick, if the send queue isn't empty, pull out and send the appropriate number of messages
			case Irc.PollQueue =>
				sendStats
				if (!sendQueue.isEmpty) processQueue else Nil

			case Irc.FlushQueue => sendQueue.clear; Nil
		}
	}
}