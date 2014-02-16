package cc.baf.irc
package io

import scala.concurrent.duration.Duration
import scala.util.Try

import java.nio.charset.Charset

import akka.actor.ActorRefFactory
import com.typesafe.config.Config
import spray.util._

case class IrcConnectionSettings(
		connectTimeout: Duration,
		maxMessageSize: Int,
		charset: String,
		throttlePenalty: Duration,
		throttleWindow: Duration
	) {

	require(connectTimeout >= Duration.Zero, "connect-timeout must be >= 0")
	require(maxMessageSize >= 0, "max-message-size must be >= 0")
	require(Try(Charset.forName(charset)).isSuccess, "charset name must be valid")
	require(throttlePenalty >= Duration.Zero && throttlePenalty.isFinite, "throttle-penalty must be >= 0 and finite")
	require(throttleWindow >= Duration.Zero && throttleWindow.isFinite, "throttle-window must be >= 0 and finite")
}

object IrcConnectionSettings extends SettingsCompanion[IrcConnectionSettings]("baf.irc.io.connection") {
	def fromSubConfig(c: Config) = {
		apply(
			c getDuration "connect-timeout",
			c getIntBytes "max-message-size",
			c getString "charset",
			c getDuration "throttle-penalty",
			c getDuration "throttle-window"
		)
	}

	def apply(optional: Option[IrcConnectionSettings])(implicit actorRefFactory: ActorRefFactory): IrcConnectionSettings = {
		optional getOrElse apply(actorSystem)
	}
}