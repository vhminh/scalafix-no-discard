package fix

import scalafix.v1._

import scala.meta.Term.{Apply, Block}
import scala.meta._

case class DiscardedFuture(position: Position) extends Diagnostic {
  override def message: String = "suspicious fire and forget Future"
}


object FutureExpr {

  val futureMatcher = SymbolMatcher.exact("scala/concurrent/Future#")

  def unapply(term: Term)(implicit doc: SemanticDocument): Option[Apply] = {
    term match {
      case apply@Apply.After_4_6_0(fun, _) =>
        fun.symbol.info.flatMap { info =>
          info.signature match {
            case MethodSignature(_, _, TypeRef(NoType, futureMatcher(_), _)) =>
              Some(apply)
            case ClassSignature(_, _, _, _) if fun.symbol.value == "scala/concurrent/Future." =>
              Some(apply)
          }
        }
    }
  }
}

class ScalafixNoDiscard extends SemanticRule("ScalafixNoDiscard") {

  override def fix(implicit doc: SemanticDocument): Patch = {
    doc.tree.collect {
      case Block(_stats) =>
        val stats = _stats.dropRight(1)
        stats.collect {
          case FutureExpr(apply) =>
            Patch.lint(DiscardedFuture(apply.pos))
          case Defn.Val(_, pats, _, FutureExpr(_)) =>
            Patch.lint(DiscardedFuture(pats.head.pos))
        }
    }.flatten.asPatch
  }

}
