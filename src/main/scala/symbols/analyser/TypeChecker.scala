package symbols.analyser

import ast.*
import diagnostics.Diagnostic
import diagnostics.Diagnostic.TypeError
import sources.SourceRange
import symbols.{BoundBlockExpr, BoundBoolLiteral, BoundBreak, BoundContinue, BoundExpr, BoundFloatLiteral, BoundIfExpr, BoundIntegerLiteral, BoundStringLiteral, BoundVariable, BoundVariableDecl, BoundWhileExpr, SemanticType, VariableSymbol}
import symbols.analyser.SemanticAnalyser.{MutableNodeToSymbol, MutableTypeRefToSemanticType, SemanticContext}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class TypeChecker(
    val semanticContext: SemanticContext
) {
  def check(expr: Expr, typeConstraint: TypeConstraint = TypeConstraint.Nothing): BoundExpr = {
    given SourceRange = expr.range
    given TypeConstraint = typeConstraint
    expr match {
      case expr: LiteralExpr => expr match {
          case BoolLiteral(value, _) => checkAndReturn(BoundBoolLiteral(value))
          case FloatLiteral(value, range) => checkAndReturn(BoundFloatLiteral(value))
          case IntegerLiteral(value, range) => checkAndReturn(BoundIntegerLiteral(value))
          case StringLiteral(value, range) => checkAndReturn(BoundStringLiteral(value))
        }
      case exprIdent: ExprIdent => {
        val optionalVariableSymbol = semanticContext.nodeToScope(expr).searchForNonOverloadableTerm(exprIdent)
        val boundVariable = optionalVariableSymbol.collect {
          // TODO - Consider the case when its an ObjectSymbol
          case variableSymbol: VariableSymbol =>
            BoundVariable(variableSymbol, varToType.getOrElse(variableSymbol, KahwaLangScope.NothingType))
        }.getOrElse(BoundVariable.ErrorVariable)
        checkWith(boundVariable.semanticType)
        boundVariable
      }
      case BinaryExpr(expr1, expr2, op, range) => ???
      case UnaryExpr(expr, op, range) => ???
      case CallExpr(callee, args, range) => ???
      case MemberAccessExpr(base, member, range) => ???
      case blockExpr: BlockExpr => {
        val boundExprs = blockExpr.exprs.map(check(_))
        val inferredType = boundExprs.lastOption.map(_.semanticType).getOrElse(KahwaLangScope.UnitType)
        checkWith(inferredType)
        BoundBlockExpr(boundExprs, inferredType)
      }
      case IfExpr(expr, ifBlock, elseBlock, range) => {
        val ifBoundExpr = check(ifBlock).asInstanceOf[BoundBlockExpr]
        val elseBoundExpr = elseBlock.map(check(_)).map(_.asInstanceOf[BoundBlockExpr])

        checkAndReturn(
          BoundIfExpr(
            check(expr, TypeConstraint.subtypeOf(KahwaLangScope.BoolType)),
            ifBoundExpr,
            elseBoundExpr,
            elseBoundExpr.map(_.semanticType typeUnion ifBoundExpr.semanticType).getOrElse(ifBoundExpr.semanticType)
          )
        )
      }
      case WhileExpr(cond, body, range) => checkAndReturn(
          BoundWhileExpr(
            check(cond, TypeConstraint.subtypeOf(KahwaLangScope.BoolType)),
            check(body).asInstanceOf[BoundBlockExpr],
            KahwaLangScope.UnitType
          )
        )
      case BreakExpr(_) => checkAndReturn(BoundBreak())
      case ContinueExpr(_) => checkAndReturn(BoundContinue())
      case LambdaExpr(paramList, body, range) => ???
      case TupleExpr(elements, range) => ???
      case VariableDecl(name, typeRef, readOnly, initExpr, range) => {
        // TODO - Assume init expr exists
        val boundInitExpr = initExpr.map(
          check(_, TypeConstraint.subtypeOf(typeRef.map(semanticContext.typeRefToSemanticType).getOrElse(KahwaLangScope.AnyType)))
        )
        val inferredType = boundInitExpr.map(_.semanticType).getOrElse(KahwaLangScope.NothingType)

        checkWith(inferredType)

        varToType += ??? // Don't know how to add it to map

        // TODO - Do something about not being able to infer type
        BoundVariableDecl(name, inferredType, readOnly, boundInitExpr)
      }
    }
  }
  
  val stack: ListBuffer[BoundBlockExpr] = ListBuffer.empty

  private val varToType: mutable.Map[VariableSymbol, SemanticType] = mutable.Map.empty

  private def checkAndReturn[T <: BoundExpr](
      boundExpr: T
  )(using typeConstraint: TypeConstraint, range: SourceRange): T = {
    semanticContext.diagnostics ++= typeConstraint.isSatisfiedBy(boundExpr.semanticType)
    boundExpr
  }

  private def checkWith(semanticType: SemanticType)(using typeConstraint: TypeConstraint, range: SourceRange): Unit = {
    semanticContext.diagnostics ++= typeConstraint.isSatisfiedBy(semanticType)
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
