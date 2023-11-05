/*
rule = ScalafixNoDiscard
 */
package fix

import scala.concurrent.Future

object PatMatching {

  def dontCareNonFutureTypes() = {
    4 match {
      case 1 => 6
      case 2 => "9"
      case _ => ???
    }
    42
  }

  def patternMatchReturn() = {
    4 match {
      case 1 => Future.successful(6)
      case 2 => Future.successful(9)
      case _ => Future.failed(???)
    }
  }

  def patternMatchIgnore() = {
    1 match { // assert: ScalafixNoDiscard
      case 1 => Future.successful(6)
      case 2 => Future.successful(9)
      case _ => Future.failed(???)
    }
    42
  }

  def patternMatchUpcast() = {
    1 match {
      case 1 => 2
      case 2 => Future.successful(9) // assert: ScalafixNoDiscard
      case _ => 3
    }
  }
}
