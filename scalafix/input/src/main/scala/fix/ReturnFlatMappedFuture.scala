/*
rule = ScalafixNoDiscard
*/
package fix

import scala.concurrent.{ExecutionContext, Future}

object ReturnFlatMappedFuture {
  def getInt()(implicit ec: ExecutionContext) = {
    Future.successful(6).flatMap { i =>
      Future.successful(9).map { j =>
        i + j
      }
    }
  }
}
