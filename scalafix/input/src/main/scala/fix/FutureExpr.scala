/*
rule = ScalafixNoDiscard
*/
package fix

import scala.concurrent.{ExecutionContext, Future}

object FutureExpr {
  implicit class FutureImplicits[T](f: Future[T])(implicit ec: ExecutionContext) {
    def >>[T2](f2: Future[T2]) = {
      f.flatMap(_ => f2)
    }
  }

  def futureSuccessful() = {
    Future.successful(6) // assert: ScalafixNoDiscard
    42
  }

  def futureConstructor()(implicit ec: ExecutionContext) = {
    Future(6) // assert: ScalafixNoDiscard
    42
  }

  def createdUsingImplicit()(implicit ec: ExecutionContext) = {
    Future.successful(6).>>(Future.successful("6")) // assert: ScalafixNoDiscard
    42
  }

  def callingAsInfixFn()(implicit ec: ExecutionContext) = {
    Future.successful(6) >> Future.successful("6") // assert: ScalafixNoDiscard
    42
  }

  def returned() = {
    Future.successful(6)
  }

  def onComplete()(implicit ec: ExecutionContext) = {
    Future.successful(6).onComplete(println)
  }

  def discardedWithTypeCast(): Unit = {
    Future.successful(6) // assert: ScalafixNoDiscard
  }

}
