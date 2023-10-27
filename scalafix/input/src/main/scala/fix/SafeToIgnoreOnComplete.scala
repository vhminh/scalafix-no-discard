/*
rule = ScalafixNoDiscard
*/
package fix

import scala.concurrent.{ExecutionContext, Future}

object SafeToIgnoreOnComplete {
  def futureWithOnComplete()(implicit ec: ExecutionContext) = {
    Future.successful(6).onComplete(println)
  }
}
