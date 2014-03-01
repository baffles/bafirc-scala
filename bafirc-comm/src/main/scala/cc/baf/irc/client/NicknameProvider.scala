package cc.baf.irc
package client

import collection.mutable.Queue

/**
 * Provides nicknames for use on connect.
 *
 * @author robertf
 */
trait NicknameProvider {
	def currentNick: String
	def currentNick_=(newNick: String): String

	def nextNick(): String
}

/**
 * Mixin to provide a basic currentNick implementation.
 *
 * @author robertf
 */
private[client] trait BasicCurrentNick {
	private var nick: String = _

	def currentNick: String = nick
	def currentNick_=(newNick: String): String = {
		nick = newNick
		currentNick
	}
}

/**
 * A NicknameProvider implementation that gives nicknames from a list of nicks.
 *
 * @author robertf
 */
class ListNicknameProvider(nicknames: List[String]) extends NicknameProvider with BasicCurrentNick {
	private val numNicks = nicknames.length
	private var curNickIdx = 0

	private var next = nicknames.head

	override def currentNick_=(newNick: String) = {
		next = newNick
		if (nicknames contains newNick) { curNickIdx = nicknames indexOf newNick; nextNick }
		super.currentNick = newNick
	}

	def nextNick() = {
		val nextListedNick = nicknames(curNickIdx)

		if (next != nextListedNick) {
			val nick = next
			next = nextListedNick
			nick
		} else {
			val n = next
			curNickIdx = (curNickIdx + 1) % numNicks
			next = nicknames(curNickIdx)
			n
		}
	}
}

/**
 * A NicknameProvider implementation that generates nicknames by gradually incrementing
 * and adding letters/numbers.
 *
 * @author robertf
 */
class GeneratedNicknameProvider(initialNick: String) extends NicknameProvider with BasicCurrentNick {
	private var next = initialNick

	override def currentNick_=(newNick: String) = {
		next = newNick
		super.currentNick = newNick
	}

	private def generateNewNick() {
		var appendNumber = false

		def incrementNick(n: List[Char]): List[Char] = n match {
			case Nil => appendNumber = true; Nil

			// nicks can't start with a digit, so this is a sanity check
			case n :: rest if rest.length > 0 && Character.isDigit(n) =>
				if (n < '9') (n + 1).toChar :: rest
				else 'A' :: rest

			case n :: rest if Character.isLowerCase(n) =>
				if (n < 'z') (n + 1).toChar :: rest
				else 'a' :: n :: rest

			case n :: rest if Character.isUpperCase(n) =>
				if (n < 'Z') (n + 1).toChar :: rest
				else 'A' :: n :: rest

			case other :: rest =>
				other :: incrementNick(rest)
		}

		next = incrementNick(next.reverse.toList).reverse.mkString
		if (appendNumber)
			next += '0'
	}

	def nextNick() = {
		val n = next
		generateNewNick
		n
	}
}

object test {
	def main(args: Array[String]) {
		val blah = new BasicCurrentNick {}

		val pro = new ListNicknameProvider("BAF" :: "Baffles" :: "[BAF]" :: "\\BAF\\" :: Nil)

		pro.currentNick = "test"

		for (i <- 0 to 10)
			println(pro.nextNick)
	}
}