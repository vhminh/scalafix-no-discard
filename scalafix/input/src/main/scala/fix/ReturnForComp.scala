/*
rule = ScalafixNoDiscard
*/
package fix

import scala.concurrent.{ExecutionContext, Future}

object ReturnForComp {
  def getInt()(implicit ec: ExecutionContext) = {
    for {
      i <- Future.successful(6)
      j <- Future.successful(9)
    } yield i + j
  }
}
