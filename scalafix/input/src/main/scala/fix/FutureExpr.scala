/*
rule = ScalafixNoDiscard
 */
package fix

import scala.concurrent.{ExecutionContext, Future}

object FutureExpr {
  private type Result[T] = Future[T]

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

  def getOrElse() = {
    Option(Future.successful(6)).getOrElse(
      Future.successful(6)
    ) // FIXME: assert: ScalafixNoDiscard, should detect getOrElse returning the same type as input arg
    42
  }

  def nullaryCreateFuture = Future.successful(6)

  def createWithNullaryMethod() = {
    nullaryCreateFuture // assert: ScalafixNoDiscard
    42
  }

  def callingAsInfixFn()(implicit ec: ExecutionContext) = {
    Future.successful(6) >> Future.successful("6") // assert: ScalafixNoDiscard
    42
  }

  def withLocalTypeDef() = {
    type LocalResult[T] = scala.concurrent.Future[T]

    def createF(): LocalResult[Int] = {
      ???
    }

    createF() // assert: ScalafixNoDiscard
    42
  }

  def withTypeDef() = {
    def createF(): Result[Int] = {
      ???
    }

    createF() // assert: ScalafixNoDiscard
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
