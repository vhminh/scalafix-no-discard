/*
rule = ScalafixNoDiscard
*/
package fix

import scala.concurrent.{ExecutionContext, Future}

object ForComps {
  def ok()(implicit ec: ExecutionContext) = {
    for {
      i <- Future.successful(6)
      j <- Future.successful(9)
    } yield i + j
  }

  def discarded()(implicit ec: ExecutionContext) = {
    for { // assert: ScalafixNoDiscard
      i <- Future.successful(6)
      j <- Future.successful(9)
    } yield i + j
    42
  }
}
