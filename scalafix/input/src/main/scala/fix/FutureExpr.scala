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

  def futureCompanionObject()(implicit ec: ExecutionContext) = {
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

  def callback(callback: (Int) => Future[Int]) = {
    callback(6) // assert: ScalafixNoDiscard
    42
  }

  def lazyValue(lazyF: => Future[Int]) = {
    lazyF // assert: ScalafixNoDiscard
    42
  }

  def onComplete()(implicit ec: ExecutionContext) = {
    Future.successful(6).onComplete(println)
  }

  def discardedWithTypeCast(): Unit = {
    Future.successful(6) // assert: ScalafixNoDiscard
  }

}
