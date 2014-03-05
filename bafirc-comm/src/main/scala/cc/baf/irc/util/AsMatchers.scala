package cc.baf.irc
package util

import scala.util.Try

object AsMatchers {
	object AsInt {
		def unapply(str: String): Option[Int] = Try(str.toInt).toOption
	}
}