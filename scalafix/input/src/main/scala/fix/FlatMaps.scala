/*
rule = ScalafixNoDiscard
*/
package fix

import scala.concurrent.{ExecutionContext, Future}

object FlatMaps {

  def withUsage()(implicit ec: ExecutionContext) = {
    Future.successful(6).flatMap { i =>
      Future.successful(9).map { j =>
        i + j
      }
    }
  }

  def discarded()(implicit ec: ExecutionContext) = {
    Future.successful(6).flatMap { i => // assert: ScalafixNoDiscard
      Future.successful(9).map { j =>
        i + j
      }
    }
    42
  }
}
