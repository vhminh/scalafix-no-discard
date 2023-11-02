/*
rule = ScalafixNoDiscard
 */
package fix

import scala.concurrent.Future

object Blocks {
  object ObjectDef {
    Future.successful(6) // assert: ScalafixNoDiscard
  }

  def branchBody() = {
    if (true) {
      Future.successful(1) // assert: ScalafixNoDiscard
      1
    } else {
      2
    }
  }

  def forCompBody() = {
    for {
      i <- Right(2)
    } yield {
      Future.successful(6) // assert: ScalafixNoDiscard
      42
    }
  }

  def tryCatchBody() = {
    try {
      Future.successful(1) // assert: ScalafixNoDiscard
      Future.successful(2)
    } catch {
      case e: Exception =>
        Future.successful(2) // assert: ScalafixNoDiscard
        Future.successful(3)
    } finally {
      Future.successful(4) // assert: ScalafixNoDiscard
      Future.successful(5) // assert: ScalafixNoDiscard
      // ^^^^^^ discarded as unit
    }
  }

}
