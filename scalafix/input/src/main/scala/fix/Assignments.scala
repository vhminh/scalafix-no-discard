/*
rule = ScalafixNoDiscard
*/
package fix

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

object Assignments {
  def assignedTo_()(implicit ec: ExecutionContext) = {
    for {
      i <- Future.successful(2)
      _ = Future(1) // assert: ScalafixNoDiscard
      _ = Future.successful(6) // assert: ScalafixNoDiscard
      _ = if (true) Future(1) else Future(2) // assert: ScalafixNoDiscard
      _ = if (true) 1 else (2)
      _ <- Future.successful(6)
    } yield i
  }
}
