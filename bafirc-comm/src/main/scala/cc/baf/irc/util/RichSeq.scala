package cc.baf.irc
package util

object RichSeq {
	/**
	 * An enriched implicit around Seq providing some missing LINQ features.
	 *
	 * @author robertf
	 */
	implicit class LinqLike[T](seq: Seq[T]) {
		def single = {
			require(seq.length == 1)
			seq.head
		}
	}
}