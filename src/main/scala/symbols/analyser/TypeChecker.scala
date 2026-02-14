package symbols.analyser

import ast.*
import diagnostics.Diagnostic
import diagnostics.Diagnostic.TypeError
import sources.SourceRange
import symbols.{
  BoundBlockExpr,
  BoundBoolLiteral,
  BoundExpr,
  BoundFloatLiteral,
  BoundIntegerLiteral,
  BoundStringLiteral,
  BoundVariable,
  BoundVariableDecl,
  SemanticType,
  VariableSymbol
}
import symbols.analyser.SemanticAnalyser.{MutableNodeToSymbol, MutableTypeRefToSemanticType}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class TypeChecker(
    val nodeToSymbol: MutableNodeToSymbol,
    val nodeToScope: NodeToScope,
    val typeRefToSemanticType: MutableTypeRefToSemanticType,
    val diagnostics: ListBuffer[Diagnostic]
) {
  def check(expr: Expr, typeConstraint: TypeConstraint = TypeConstraint.Nothing): BoundExpr = {
    given SourceRange = expr.range
    given TypeConstraint = typeConstraint
    expr match {
      case expr: LiteralExpr => expr match {
          case BoolLiteral(value, _) => {
            checkWith(KahwaLangScope.BoolType)
            BoundBoolLiteral(value)
          }
          case FloatLiteral(value, range) => {
            checkWith(KahwaLangScope.FloatType)
            BoundFloatLiteral(value)
          }
          case IntegerLiteral(value, range) => {
            checkWith(KahwaLangScope.IntType)
            BoundIntegerLiteral(value)
          }
          case StringLiteral(value, range) => {
            checkWith(KahwaLangScope.StringType)
            BoundStringLiteral(value)
          }
        }
      case exprIdent: ExprIdent => {
        val termSymbols = nodeToScope(expr).searchForTerm(exprIdent).collect { case symbol: VariableSymbol => symbol }
        if (termSymbols.isEmpty) {
          ???
        } else {
          BoundVariable(termSymbols.head, varToType.getOrElse(termSymbols.head, KahwaLangScope.NothingType))
        }
      }
      case BinaryExpr(expr1, expr2, op, range) => ???
      case UnaryExpr(expr, op, range) => ???
      case CallExpr(callee, args, range) => ???
      case MemberAccessExpr(base, member, range) => ???
      case BlockExpr(exprs, range) => {
        val boundExprs = exprs.map(check(_))
        val inferredType = boundExprs.lastOption.map(_.semanticType).getOrElse(KahwaLangScope.UnitType)
        checkWith(inferredType)
        BoundBlockExpr(boundExprs, inferredType)
      }
      case IfExpr(expr, ifBlock, elseBlock, range) => ???
      case WhileExpr(cond, body, range) => ???
      case BreakExpr(range) => ???
      case ContinueExpr(range) => ???
      case LambdaExpr(paramList, body, range) => ???
      case TupleExpr(elements, range) => ???
      case VariableDecl(name, typeRef, readOnly, initExpr, range) => {
        // TODO - Assume init expr exists
        val boundInitExpr = initExpr.map(
          check(_, TypeConstraint.subtypeOf(typeRef.map(typeRefToSemanticType).getOrElse(KahwaLangScope.AnyType)))
        )
        val inferredType = boundInitExpr.map(_.semanticType).getOrElse(KahwaLangScope.NothingType)

        checkWith(inferredType)

        varToType += ??? // Don't know how to add it to map

        // TODO - Do something about not being able to infer type
        BoundVariableDecl(name, inferredType, readOnly, boundInitExpr)
      }
    }
  }

  private val varToType: mutable.Map[VariableSymbol, SemanticType] = mutable.Map.empty

  private def checkWith(semanticType: SemanticType)(using typeConstraint: TypeConstraint, range: SourceRange): Unit = {
    diagnostics ++= typeConstraint.isSatisfiedBy(semanticType)
  }

  case class TypeConstraint(subTypeOf: SemanticType, superTypeOf: SemanticType) {
    def isSatisfiedBy(semanticType: SemanticType)(using range: SourceRange): Option[Diagnostic] = {
      if (semanticType < subTypeOf && superTypeOf < semanticType) {
        None
      } else {
        ??? // TODO - Should have both superTypeOf and subtypeOf
        Some(TypeError(semanticType, superTypeOf, range))
      }
    }
  }

  object TypeConstraint {
    def subtypeOf(semanticType: SemanticType): TypeConstraint =
      TypeConstraint(semanticType, KahwaLangScope.NothingType)

    def supertypeOf(semanticType: SemanticType): TypeConstraint =
      TypeConstraint(KahwaLangScope.AnyType, semanticType)

    val Nothing = TypeConstraint(KahwaLangScope.AnyType, KahwaLangScope.NothingType)
  }
}
