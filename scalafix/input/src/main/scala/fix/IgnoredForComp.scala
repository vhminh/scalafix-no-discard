/*
rule = ScalafixNoDiscard
*/
package fix

import scala.concurrent.{ExecutionContext, Future}

object IgnoredForComp {
  def getInt()(implicit ec: ExecutionContext) = {
    for {
      i <- Future.successful(6)
      j <- Future.successful(9)
    } yield i + j // assert: ScalafixNoDiscard
    42
  }
}
