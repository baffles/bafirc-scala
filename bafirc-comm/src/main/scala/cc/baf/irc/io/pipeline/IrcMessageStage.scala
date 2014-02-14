package cc.baf.irc
package io.pipeline

import akka.io._
import akka.util._

import cc.baf.irc.data.Message
import cc.baf.irc.parser.IrcMessageParser

/**
 * akka-io pipeline stage for IRC messages. It translates between `Message` objects and plain strings.
 *
 * @author robertf
 */
private[io] class IrcMessageStage extends SymmetricPipelineStage[PipelineContext, Message, String] {

	def apply(context: PipelineContext) = new SymmetricPipePair[Message, String] {
		val parser = new IrcMessageParser

		def commandPipeline = { message: Message =>
			println(s"-> $message")
			context.singleCommand(message.toIrc)
		}

		/** Pipeline for receiving messages. Throws if an incoming message is greater than `maxSize`. */
		def eventPipeline = { message: String =>
			context.singleEvent(parser parseMessage message)
		}
	}
}