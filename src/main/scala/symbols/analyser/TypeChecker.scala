package symbols.analyser

import ast.*
import diagnostics.Diagnostic
import symbols.{BoundExpr, SemanticType}
import symbols.analyser.SemanticAnalyser.{MutableExprToType, MutableIdentToSymbol, MutableNodeToSymbol, MutableTypeRefToSemanticType}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class TypeChecker(
    val nodeToSymbol: MutableNodeToSymbol,
    val nodeToScope: NodeToScope,
    val typeRefToSemanticType: MutableTypeRefToSemanticType,
    val diagnostics: ListBuffer[Diagnostic]
) {
  
  def check(expr: Expr): BoundExpr = {
    expr match {
      case expr: LiteralExpr => ???
      case Ident(head, tail, range) => ???
      case BinaryExpr(expr1, expr2, op, range) => ???
      case UnaryExpr(expr, op, range) => ???
      case CallExpr(callee, args, range) => ???
      case MemberAccessExpr(base, member, range) => ???
      case BlockExpr(exprs, range) => ???
      case IfExpr(expr, ifBlock, elseBlock, range) => ???
      case WhileExpr(cond, body, range) => ???
      case BreakExpr(range) => ???
      case ContinueExpr(range) => ???
      case LambdaExpr(paramList, body, range) => ???
      case TupleExpr(elements, range) => ???
      case VariableDecl(name, typeRef, readOnly, initExpr, range) => ???
    }
  }

  private val constraints: mutable.Map[Expr, TypeConstraint] = mutable.Map()

  private case class TypeConstraint(subtypeOf: SemanticType, superTypeOf: SemanticType)

  private object TypeConstraint {
    def subtypeOf(semanticType: SemanticType): TypeConstraint =
      TypeConstraint(semanticType, KahwaLangScope.NothingType)

    def supertypeOf(semanticType: SemanticType): TypeConstraint =
      TypeConstraint(KahwaLangScope.AnyType, semanticType)
  }
}
