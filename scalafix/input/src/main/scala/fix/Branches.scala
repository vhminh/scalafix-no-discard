/*
rule = ScalafixNoDiscard
 */
package fix

import scala.concurrent.{ExecutionContext, Future}

object Branches {

  def returnFutureBothBranches() = {
    if (true) {
      Future.successful(6)
    } else {
      Future.successful(9)
    }
  }

  def discardFutureBothBranches() = {
    if (true) { // assert: ScalafixNoDiscard
      Future.successful(6)
    } else {
      Future.successful(9)
    }
    42
  }

  def companionAndClassesAreConsideredTheSame(implicit ec: ExecutionContext) = {
    if (true) {
      Future.successful(6)
    } else {
      Future(9)
    }
  }

  def discardedWithUpcast() = {
    if (true) {
      Future.successful(6) // assert: ScalafixNoDiscard
    } else {
      9
    }
    42
  }

  def nothingExtendsFuture() = {
    if (true) {
      Future.successful(6)
    } else {
      ???
    }
  }

}
