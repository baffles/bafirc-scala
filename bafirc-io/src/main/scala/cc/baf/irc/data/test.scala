package cc.baf.irc
package data

object test {
	def main(args: Array[String]) {
		val pm = Messages.PrivMsg("Sev", "you suck")
		println(pm)

		pm match {
			case Messages.PrivMsg(_, target :: Nil, text) => println(s"-> $target: $text")
			case _ => println("wtf?")
		}
	}
}