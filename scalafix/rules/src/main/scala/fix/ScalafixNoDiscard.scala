package fix

import scalafix.v1._

import scala.meta.{Defn, Position, Term}
import scala.meta.Term.{Apply, ApplyInfix, Block, If}

case class DiscardedFuture(position: Position) extends Diagnostic {
  override def message: String = "suspicious fire and forget Future"
}

object RetTerm {
  def unapply(term: Term): Option[Term] = {
    term match {
      case Block(stats) =>
        stats.lastOption match {
          case Some(term: Term) => Some(term)
          case _ => None
        }
      case other => Some(other)
    }
  }
}

object SemType {
  def unapply(term: Term)(implicit doc: SemanticDocument): Option[Symbol] = {
    term match {
      case Apply.After_4_6_0(fun, _) =>
        fun.symbol.info.flatMap { info =>
          info.signature match {
            case MethodSignature(_, _, TypeRef(NoType, typeSymbol, _)) =>
              Some(typeSymbol)
            case ClassSignature(_, _, _, _) =>
              Some(fun.symbol)
            case _ => None
          }
        }
      case ApplyInfix.After_4_6_0(_, fun, _, _) =>
        fun.symbol.info.flatMap { info =>
          info.signature match {
            case MethodSignature(_, _, TypeRef(NoType, typeSymbol, _)) =>
              Some(typeSymbol)
            case _ => None
          }
        }
      case If.After_4_4_0(_, thenTerm, elseTerm, _) =>
        (thenTerm, elseTerm) match {
          case (SemType(thenType), SemType(elseType)) if thenType == elseType =>
            Some(thenType)
          case _ => None // FIXME: check subtypes
        }
      case RetTerm(retTerm) =>
        if (retTerm != term) {
          unapply(retTerm)
        } else {
          None
        }
    }
  }
}

object FutureExpr {

  val futureMatcher = SymbolMatcher.exact("scala/concurrent/Future#")
  val futureCompanionMatcher = SymbolMatcher.exact("scala/concurrent/Future.")

  def unapply(term: Term)(implicit doc: SemanticDocument): Option[Symbol] = {
    term match {
      case SemType(futureMatcher(s)) => Some(s)
      case SemType(futureCompanionMatcher(s)) => Some(s)
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
          case expr@FutureExpr(_) =>
            Patch.lint(DiscardedFuture(expr.pos))
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
          case Some(expr@FutureExpr(_)) if implicitUnits.contains(expr.pos) =>
            Some(Patch.lint(DiscardedFuture(expr.pos)))
          case _ =>
            None
        }
    }.flatten.asPatch
  }

  private def upcasted(implicit doc: SemanticDocument): Patch = {
    doc.tree.collect {
      case Term.If.After_4_4_0(_, thenBranch, elseBranch, _) =>
        (thenBranch, elseBranch) match {
          // TODO: check Future convertable types
          case (FutureExpr(_), FutureExpr(_)) => None
          case (RetTerm(expr@FutureExpr(_)), _) => Some(Patch.lint(DiscardedFuture(expr.pos)))
          case (_, RetTerm(expr@FutureExpr(_))) => Some(Patch.lint(DiscardedFuture(expr.pos)))
          case _ => None
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
    Seq(unassignedIntermediateExpr, implicitlyDiscardedAsUnits, upcasted, assignedToUnusedVar).asPatch
  }
}
