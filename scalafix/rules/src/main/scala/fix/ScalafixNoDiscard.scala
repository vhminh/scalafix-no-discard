package fix

import scalafix.v1._

import scala.meta.{Defn, Enumerator, Pat, Position, Template, Term}
import scala.meta.Term.{Apply, ApplyInfix, Block, ForYield, If, Match}

case class BranchReturnTypeCast(branchType: Symbol, finalType: Symbol, position: Position) extends Diagnostic {
  override def message: String = {
    s"value of type ${branchType.displayName} is maybe discarded, because the branch expr returns ${finalType.displayName}"
  }
}

case class IntermediateValueDiscarded(`type`: Symbol, position: Position) extends Diagnostic {
  override def message: String = s"intermediate value of type ${`type`.displayName} is discarded"
}

object Symbols {
  val Unit = Symbol("scala/Unit#")
  val Nothing = Symbol("scala/Nothing#")
  val Any = Symbol("scala/Any#")
}

object utils {
  def lastOfBlock(term: Term): Term = {
    term match {
      case Block(_ :+ (lastTerm: Term)) => lastTerm
      case other => other
    }
  }
}

object TypeOf {

  val scalaFnMatcher = SymbolMatcher.exact((for (i <- 0 to 22) yield s"scala/Function$i#"): _*)

  object TypeSym {
    def unapply(semType: SemanticType): Option[Symbol] = {
      semType match {
        case TypeRef(_, symbol, _) => Some(symbol)
        case _ => None
      }
    }
  }

  def subtype(type0: Symbol, types: Symbol*): Symbol = {
    // FIXME: need to look up for parents, only consider Any and Nothing for now
    if (types.exists(t => t != type0 && t != Symbols.Nothing)) {
      Symbols.Any
    } else {
      type0
    }
  }

  def subtype(types: Seq[Symbol]): Symbol = {
    assert(types.nonEmpty)
    subtype(types.head, types.tail: _*)
  }

  def resolveTypeAlias(symbol: Symbol)(implicit doc: SemanticDocument): Symbol = {
    symbol.info.flatMap { info =>
      info.signature match {
        case TypeSignature(_, TypeSym(lowerBound), TypeSym(upperBound)) if lowerBound == upperBound =>
          // FIXME: handle lowerBound != upperBound
          Some(lowerBound)
        case _ => None
      }
    }.getOrElse(symbol)
  }

  def unapply(term: Term)(implicit doc: SemanticDocument): Option[Symbol] = {
    val symbol = term match {
      case term@Term.Name(_) =>
        term.symbol.info.flatMap { info =>
          info.signature match {
            case ValueSignature(ByNameType(TypeSym(symbol))) =>
              Some(symbol)
            case ValueSignature(TypeSym(symbol)) =>
              Some(symbol)
            case MethodSignature(_, _, TypeSym(retType)) =>
              Some(retType)
            case _ => None
          }
        }
      case Apply.After_4_6_0(fun, _) =>
        fun.symbol.info.flatMap { info =>
          info.signature match {
            case MethodSignature(_, _, TypeSym(symbol)) =>
              Some(symbol)
            case ClassSignature(_, _, _, decls) =>
              val applyRetTypes = decls.flatMap { s =>
                if (s.displayName == "apply") {
                  s.signature match {
                    case MethodSignature(_, _, TypeSym(symbol)) =>
                      // FIXME: assume that all .apply() method return the type associate with this companion object
                      // FIXME: proper check for param types to determine the correct .apply()
                      Some(symbol)
                    case _ => None
                  }
                } else {
                  None
                }
              }
              applyRetTypes.headOption
            case ValueSignature(TypeRef(NoType, scalaFnMatcher(_), _ :+ TypeSym(symbol))) =>
              Some(symbol)
            case _ => None
          }
        }
      case ApplyInfix.After_4_6_0(_, fun, _, _) =>
        fun.symbol.info.flatMap { info =>
          info.signature match {
            case MethodSignature(_, _, TypeSym(symbol)) =>
              Some(symbol)
            case _ => None
          }
        }
      case If.After_4_4_0(_, thenTerm, elseTerm, _) =>
        (thenTerm, elseTerm) match {
          case (TypeOf(thenType), TypeOf(elseType)) =>
            Some(subtype(thenType, elseType))
          case _ => None
        }
      case ForYield(Enumerator.Generator(_, TypeOf(rhsType)) :: _, body) =>
        // FIXME: we only need to know that it is a Future, need to check body for cases such as Future[Int]
        Some(rhsType)
      case Match.After_4_4_5(_, cases, _) =>
        val caseTypeOpts = cases.map { xcase => TypeOf.unapply(xcase.body) }
        if (caseTypeOpts.contains(None)) {
          None
        } else {
          Some(subtype(caseTypeOpts.flatten))
        }
      case Block(_ :+ (last: Term)) =>
        unapply(last)
      case _ => None
    }
    symbol.map(resolveTypeAlias)
  }
}

object FutureExpr {

  private val futureMatcher = SymbolMatcher.exact("scala/concurrent/Future#")
  private val futureEitherMatcher = SymbolMatcher.exact("com/evidence/service/common/monad/FutureEither#")

  private val allMatchers = futureMatcher + futureEitherMatcher

  def isFuture(sym: Symbol): Boolean = {
    allMatchers.matches(sym)
  }

  def unapply(term: Term)(implicit doc: SemanticDocument): Option[Symbol] = {
    term match {
      case TypeOf(allMatchers(symbol)) => Some(symbol)
      case _ => None
    }
  }
}

class ScalafixNoDiscard extends SemanticRule("ScalafixNoDiscard") {

  private def unassignedIntermediateExpr(implicit doc: SemanticDocument): Patch = {
    doc.tree
      .collect {
        case Block(_stats) =>
          val stats = _stats.dropRight(1)
          stats.collect { case expr@FutureExpr(typeSym) =>
            Patch.lint(IntermediateValueDiscarded(typeSym, expr.pos))
          }
        case Defn.Object(_, _, Template.After_4_4_0(_, _, _, body, _)) =>
          body.collect { case expr@FutureExpr(typeSym) =>
            Patch.lint(IntermediateValueDiscarded(typeSym, expr.pos))
          }
      }
      .flatten
      .asPatch
  }

  private def implicitlyDiscardedAsUnits(implicit doc: SemanticDocument): Patch = {
    val implicitUnits = doc.diagnostics
      .filter(_.message.startsWith("discarded non-Unit value"))
      .map(_.position)
      .toSet
    doc.tree
      .collect { case Block(stats) =>
        stats.lastOption match {
          case Some(expr@FutureExpr(xtype)) if implicitUnits.contains(expr.pos) =>
            Some(Patch.lint(IntermediateValueDiscarded(xtype, expr.pos)))
          case _ =>
            None
        }
      }
      .flatten
      .asPatch
  }

  private def branchTypeCasted(terms: Seq[Term])(implicit doc: SemanticDocument): Patch = {
    assert(terms.nonEmpty)
    val types = terms
      .map(TypeOf.unapply(_))
      .zipWithIndex
      .flatMap {
        case (Some(t), i) => Some(i -> t)
        case _ => None
      }
      .toMap
    val subType = if (types.size < terms.length) {
      Symbols.Any
    } else {
      TypeOf.subtype(types.values.toSeq)
    }
    if (types.values.exists(FutureExpr.isFuture) && !FutureExpr.isFuture(subType)) {
      val termArr = terms.toIndexedSeq
      val indices = types.filter(tuple => FutureExpr.isFuture(tuple._2)).keys
      val patches =
        for (idx <- indices)
          yield Patch.lint(
            BranchReturnTypeCast(
              types(idx),
              Symbols.Any,
              utils.lastOfBlock(termArr(idx)).pos,
            )
          )
      patches.asPatch
    } else {
      Patch.empty
    }
  }

  private def branchTypeCasted(implicit doc: SemanticDocument): Patch = {
    doc.tree.collect {
      case Term.If.After_4_4_0(_, thenBranch, elseBranch, _) =>
        branchTypeCasted(Seq(thenBranch, elseBranch))
      case Match.After_4_4_5(_, cases, _) =>
        branchTypeCasted(cases.map(_.body))
      // TODO: check try-catch
    }.asPatch
  }

  private def assignedToUnusedVarInForComps(implicit doc: SemanticDocument): Patch = {
    doc.tree.collect { case ForYield(enums, _) =>
      enums.collect { case Enumerator.Val(wildcard@Pat.Wildcard(), FutureExpr(typeSym)) =>
        Patch.lint(IntermediateValueDiscarded(typeSym, wildcard.pos))
      }.asPatch
    }.asPatch
  }

  override def fix(implicit doc: SemanticDocument): Patch = {
    Seq(
      unassignedIntermediateExpr,
      implicitlyDiscardedAsUnits,
      branchTypeCasted,
      assignedToUnusedVarInForComps,
    ).asPatch
  }
}
