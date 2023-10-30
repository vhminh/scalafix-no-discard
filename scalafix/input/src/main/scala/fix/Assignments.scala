/*
rule = ScalafixNoDiscard
*/
package fix

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

object Assignments {
  def withUsage()(implicit ec: ExecutionContext) = {
    val future = Future.successful(6)
    Await.result(future, Duration.Inf)
    42
  }

  def discarded()(implicit ec: ExecutionContext) = {
    val future = Future(6) // assert: ScalafixNoDiscard
    42
  }
}
