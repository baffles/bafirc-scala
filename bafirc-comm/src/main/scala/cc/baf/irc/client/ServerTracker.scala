package cc.baf.irc
package client

/**
 * An implementation detail - keeps track of the configured servers for an `IrcClient` and figuring out which one to
 * connect to.
 *
 * @author robertf
 */
private[client] class ServerTracker(serverList: List[IrcServer]) {
	require(!serverList.isEmpty, "serverList must not be empty")

	private val numServers = serverList.length
	private var curServerIdx = 0

	def currentServer = serverList(curServerIdx)

	def newServer() {
		curServerIdx = (curServerIdx + 1) % numServers
	}
}