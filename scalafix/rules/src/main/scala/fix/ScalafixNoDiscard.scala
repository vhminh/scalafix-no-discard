package fix

import scalafix.v1._

import scala.meta.Term.{Apply, ApplyInfix, Block}
import scala.meta._

case class DiscardedFuture(position: Position) extends Diagnostic {
  override def message: String = "suspicious fire and forget Future"
}


object FutureExpr {

  val futureMatcher = SymbolMatcher.exact("scala/concurrent/Future#")

  def unapply(term: Term)(implicit doc: SemanticDocument): Option[Term] = {
    term match {
      case apply@Apply.After_4_6_0(fun, _) =>
        fun.symbol.info.flatMap { info =>
          info.signature match {
            case MethodSignature(_, _, TypeRef(NoType, futureMatcher(_), _)) =>
              Some(apply)
            case ClassSignature(_, _, _, _) if fun.symbol.value == "scala/concurrent/Future." =>
              Some(apply)
            case _ => None
          }
        }
      case apply@ApplyInfix.After_4_6_0(_, fun, _, _) =>
        fun.symbol.info.flatMap { info =>
          info.signature match {
            case MethodSignature(_, _, TypeRef(NoType, futureMatcher(_), _)) =>
              Some(apply)
            case _ => None
          }
        }
      case _ => None
    }
  }
}

class ScalafixNoDiscard extends SemanticRule("ScalafixNoDiscard") {

  private def unassignedIntermediateExpr(implicit doc: SemanticDocument): Patch = {
    doc.tree.collect {
      case Block(_stats) =>
        val stats = _stats.dropRight(1)
        stats.collect {
          case FutureExpr(apply) =>
            Patch.lint(DiscardedFuture(apply.pos))
        }
    }.flatten.asPatch
  }

  private def implicitlyDiscardedAsUnits(implicit doc: SemanticDocument): Patch = {
    val implicitUnits = doc.diagnostics
      .filter(_.message.startsWith("discarded non-Unit value"))
      .map(_.position)
      .toSet
    doc.tree.collect {
      case Block(stats) =>
        stats.lastOption match {
          case Some(FutureExpr(last)) if implicitUnits.contains(last.pos) =>
            Some(Patch.lint(DiscardedFuture(last.pos)))
          case _ =>
            None
        }
    }.flatten.asPatch
  }

  private def assignedToUnusedVar(implicit doc: SemanticDocument): Patch = {
    doc.tree.collect {
      case Defn.Val(_, pats, _, FutureExpr(_)) =>
        // Patch.lint(DiscardedFuture(pats.head.pos))
        Patch.empty
    }.asPatch
  }

  override def fix(implicit doc: SemanticDocument): Patch = {
    Seq(unassignedIntermediateExpr, implicitlyDiscardedAsUnits, assignedToUnusedVar).asPatch
  }
}
