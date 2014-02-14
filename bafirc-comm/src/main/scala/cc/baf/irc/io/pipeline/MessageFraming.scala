package cc.baf.irc
package io.pipeline

import java.nio.charset.Charset

import scala.annotation.tailrec

import akka.io._
import akka.util._

/**
 * akka-io pipeline stage for message frames. It will assemble complete messages from an incoming byte stream with the
 * given `delimiters` and `charset`.
 *
 * This implementation is geared toward handling IRC messages, so the default parameters match those laid out in the
 * IRC spec, but they can easily be changed.
 *
 * Message length (by default) is limited to 512 bytes after delimitation (as per RFC-1459, section 2.3). RFC specifies
 * ASCII, though we default to UTF-8 encoding.
 *
 * @author robertf
 */
private[io] class MessageFraming(
		maxSize: Int = 512,
		charset: String = "UTF-8",
		delimiters: List[Byte] = List[Byte]('\r', '\n')
	) extends SymmetricPipelineStage[PipelineContext, String, ByteString] {

	def apply(context: PipelineContext) = new SymmetricPipePair[String, ByteString] {
		var buffer = None: Option[ByteString]
		val delimitersBS = ByteString(delimiters: _*)

		/**
		 * Extract as many complete messages as possible from the given ByteString. Returns the remainder along with a
		 * list of the extracted messages.
		 */
		private def extractMessages(bs: ByteString) = {
			@tailrec def recurse(bs: ByteString, acc: List[String]): (Option[ByteString], List[String]) = {
				if (bs.isEmpty) None -> acc
				else {
					val delim = bs.indexWhere { delimiters contains _ }
					if (delim >= 0) {
						// we have at least one message... slice it out and get the remainder
						val messageRaw = bs.slice(0, delim)
						val rest = bs.drop(delim + 1).dropWhile { delimiters contains _ }

						// decode the message
						val messageDecoded = messageRaw decodeString charset

						recurse(rest, messageDecoded :: acc)
					} else {
						// no complete messages in the buffer
						Some(bs.compact) -> acc
					}
				}
			}

			val (rem, acc) = recurse(bs, Nil)
			rem -> acc.reverse.filterNot(_.isEmpty)
		}

		/** Pipeline for sending messages. Silently drops messages that are greater than `maxSize`. */
		def commandPipeline = { message: String =>
			val messageEncoded = ByteString(message, charset) ++ delimitersBS
			if (messageEncoded.length > maxSize) Seq()
			else context.singleCommand(messageEncoded)
		}

		/** Pipeline for receiving messages. Throws if an incoming message is greater than `maxSize`. */
		def eventPipeline = { bs: ByteString =>
			val data = buffer.map(_ ++ bs) getOrElse bs
			val (newBuffer, messages) = extractMessages(data)
			buffer = newBuffer

			messages match {
				case Nil => Nil
				case one :: Nil => context.singleEvent(one)
				case many => many map { Left(_) }
			}
		}
	}
}