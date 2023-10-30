/*
rule = ScalafixNoDiscard
*/
package fix

import scala.concurrent.{ExecutionContext, Future}

object Blocks {
  Future.successful(6) // assert: ScalafixNoDiscard
}
