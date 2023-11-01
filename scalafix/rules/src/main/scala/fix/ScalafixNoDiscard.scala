package fix

import scalafix.v1._

import scala.meta.{Defn, Enumerator, Position, Term}
import scala.meta.Term.{Apply, ApplyInfix, Block, ForYield, If}

case class CustomDiagnostic(message: String, position: Position) extends Diagnostic

case class Discarded(`type`: Symbol, position: Position) extends Diagnostic {
  override def message: String = s"suspicious discarded value of type ${`type`.displayName}"
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
  def typeEqIgnoreCompanion(type1: Symbol, type2: Symbol): Boolean = {
    val s1 = if (type1.value.endsWith("#") || type1.value.endsWith(".")) {
      type1.value.dropRight(1)
    }
    val s2 = if (type2.value.endsWith("#") || type2.value.endsWith(".")) {
      type2.value.dropRight(1)
    }
    s1 == s2
  }

  val scalaFnMatcher = SymbolMatcher.exact((for (i <- 0 to 22) yield s"scala/Function$i#"): _*)

  def unapply(term: Term)(implicit doc: SemanticDocument): Option[Symbol] = {
    term match {
      case term@Term.Name(_) =>
        term.symbol.info.flatMap { info =>
          info.signature match {
            case ValueSignature(ByNameType(TypeRef(NoType, typeSymbol, _))) =>
              Some(typeSymbol)
            case _ => None
          }
        }
      case Apply.After_4_6_0(fun, _) =>
        fun.symbol.info.flatMap { info =>
          info.signature match {
            case MethodSignature(_, _, TypeRef(NoType, typeSymbol, _)) =>
              Some(typeSymbol)
            case ClassSignature(_, _, _, _) =>
              Some(fun.symbol)
            case ValueSignature(TypeRef(NoType, scalaFnMatcher(_), _ :+ TypeRef(NoType, typeSym, _))) =>
              Some(typeSym)
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
          case (SemType(thenType), SemType(elseType)) if typeEqIgnoreCompanion(thenType, elseType) =>
            Some(thenType)
          case _ => None // FIXME: check subtypes
        }
      case ForYield(Enumerator.Generator(_, SemType(rhsType)) :: _, body) =>
        Some(rhsType) // FIXME: we only need to know that it is a Future, need to check body for cases such as Future[Int]
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
  val futureEitherMatcher = SymbolMatcher.exact("com/evidence/service/common/monad/FutureEither#")
  val futureEitherCompanionMatcher = SymbolMatcher.exact("com/evidence/service/common/monad/FutureEither.")

  val allMatchers = futureMatcher + futureCompanionMatcher + futureEitherMatcher + futureEitherCompanionMatcher

  def unapply(term: Term)(implicit doc: SemanticDocument): Option[Symbol] = {
    term match {
      case SemType(allMatchers(s)) => Some(s)
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
          case expr@FutureExpr(xtype) =>
            Patch.lint(Discarded(xtype, expr.pos))
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
          case Some(expr@FutureExpr(xtype)) if implicitUnits.contains(expr.pos) =>
            Some(Patch.lint(Discarded(xtype, expr.pos)))
          case _ =>
            None
        }
    }.flatten.asPatch
  }

  private def upcasted(implicit doc: SemanticDocument): Patch = {
    doc.tree.collect {
      case ifExpr@Term.If.After_4_4_0(_, thenBranch, elseBranch, _) =>
        (thenBranch, elseBranch) match {
          // TODO: check Future convertable types
          case (FutureExpr(f1), FutureExpr(f2)) =>
            if (!SemType.typeEqIgnoreCompanion(f1, f2)) {
              Some(Patch.lint(CustomDiagnostic(s"${f1.displayName} and ${f2.displayName} are Futures but are upcasted, please make sure that their values are not discarded", ifExpr.pos)))
            } else {
              None
            }
          case (RetTerm(expr@FutureExpr(xtype)), _) => Some(Patch.lint(Discarded(xtype, expr.pos)))
          case (_, RetTerm(expr@FutureExpr(xtype))) => Some(Patch.lint(Discarded(xtype, expr.pos)))
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
