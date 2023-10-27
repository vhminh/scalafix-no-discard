/*
rule = ScalafixNoDiscard
*/
package fix

import scala.concurrent.Future

object IgnoredFuture {
  Future.successful(6) // assert: ScalafixNoDiscard
}
