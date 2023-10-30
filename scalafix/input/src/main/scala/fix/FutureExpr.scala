/*
rule = ScalafixNoDiscard
*/
package fix

import scala.concurrent.{ExecutionContext, Future}

object FutureExpr {
  def futureSuccessful() = {
    Future.successful(6) // assert: ScalafixNoDiscard
    42
  }

  def futureConstructor()(implicit ec: ExecutionContext) = {
    Future(6) // assert: ScalafixNoDiscard
    42
  }

  def returned() = {
    Future.successful(6)
  }

  def onComplete()(implicit ec: ExecutionContext) = {
    Future.successful(6).onComplete(println)
  }

  def discardedWithTypeCast(): Unit = {
    Future.successful(6) // assert:ScalafixNoDiscard
  }

}
