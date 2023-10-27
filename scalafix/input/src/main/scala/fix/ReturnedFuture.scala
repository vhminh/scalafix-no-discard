/*
rule = ScalafixNoDiscard
*/
package fix

import scala.concurrent.Future

object ReturnedFuture {
  def query(): Future[Int] = {
    Future.successful(6)
  }
}
