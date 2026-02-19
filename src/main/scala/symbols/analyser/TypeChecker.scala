package symbols.analyser

import ast.*
import diagnostics.Diagnostic
import diagnostics.Diagnostic.{SymbolAlreadyDeclared, TypeError}
import sources.SourceRange
import symbols.*
import symbols.analyser.SemanticAnalyser.SemanticContext

import scala.collection.mutable.ListBuffer

class TypeChecker(
    val semanticContext: SemanticContext
) {
  def check(expr: Expr, typeConstraint: TypeConstraint = TypeConstraint.Nothing): BoundExpr = {
    given SourceRange = expr.range
    given TypeConstraint = typeConstraint
    expr match {
      // Literal expressions directly convert to bound literals
      case expr: LiteralExpr => expr match {
          case BoolLiteral(value, _) => checkAndReturn(BoundBoolLiteral(value))
          case FloatLiteral(value, range) => checkAndReturn(BoundFloatLiteral(value))
          case IntegerLiteral(value, range) => checkAndReturn(BoundIntegerLiteral(value))
          case StringLiteral(value, range) => checkAndReturn(BoundStringLiteral(value))
        }
      // Identifiers are either variables (with possible chained field access) or objects or TODO - ???
      case exprIdent: ExprIdent => {
        val optionalVariableSymbol = semanticContext.nodeToEnclosingScope(expr).searchForNonOverloadableTerm(exprIdent)
        val boundVariable = optionalVariableSymbol.collect {
          // TODO - Consider the case when its an ObjectSymbol
          case variableSymbol: VariableSymbol =>
            // TODO - Assume variable symbol has inferred type, which is true for all local variables but not sure about global variables
            //      - Maybe need to type check them first
            BoundVariable(variableSymbol, variableSymbol.semanticType)
        }.getOrElse(BoundVariable.ErrorVariable)
        checkWith(boundVariable.semanticType)
        boundVariable
      }
      case BinaryExpr(expr1, expr2, op, range) => op match {
          case BinaryOp.EQUALS => ???
          case BinaryOp.DOUBLE_EQUALS => ???
          case BinaryOp.LESS => ???
          case BinaryOp.GREATER => ???
          case BinaryOp.LESS_EQUALS => ???
          case BinaryOp.GREATER_EQUALS => ???
          case BinaryOp.NOT_EQUALS => ???
          case BinaryOp.PLUS => ???
          case BinaryOp.MINUS => ???
          case BinaryOp.STAR => ???
          case BinaryOp.SLASH => ???
          case BinaryOp.MODULO => ???
          case BinaryOp.PLUS_EQUALS => ???
          case BinaryOp.MINUS_EQUALS => ???
          case BinaryOp.STAR_EQUALS => ???
          case BinaryOp.SLASH_EQUALS => ???
          case BinaryOp.MODULO_EQUALS => ???
          case BinaryOp.LEFT_SHIFT_EQUALS => ???
          case BinaryOp.RIGHT_SHIFT_EQUALS => ???
          case BinaryOp.BITWISE_AND_EQUALS => ???
          case BinaryOp.BITWISE_OR_EQUALS => ???
          case BinaryOp.BITWISE_XOR_EQUALS => ???
          case BinaryOp.LOGICAL_AND => ???
          case BinaryOp.LOGICAL_OR => ???
          case BinaryOp.BITWISE_AND => ???
          case BinaryOp.BITWISE_OR => ???
          case BinaryOp.BITWISE_XOR => ???
          case BinaryOp.LEFT_SHIFT => ???
          case BinaryOp.RIGHT_SHIFT => ???
        }
      case UnaryExpr(expr, op, range) => op match {
          case UnaryOp.NOT => ???
          case UnaryOp.PLUS => ???
          case UnaryOp.MINUS => ???
          case UnaryOp.POST_INCREMENT => ???
          case UnaryOp.POST_DECREMENT => ???
          case UnaryOp.PRE_INCREMENT => ???
          case UnaryOp.PRE_DECREMENT => ???
        }
      case CallExpr(callee, args, range) => callee match {
          case exprIdent: ExprIdent => {
            val boundArgs = args.map(check(_))
            val optionalFunctions = semanticContext.nodeToEnclosingScope(expr).searchForOverloadableTerm(exprIdent)
            // TODO - Ignore for now the possibility of a term with apply methods
            val validCandidates = optionalFunctions
              .map(_.collect {
                case functionSymbol: FunctionSymbol if funcValid(functionSymbol, boundArgs.map(_.semanticType)) =>
                  functionSymbol
              })
              .toList
              .flatten // TODO - Ignore out of scope function for now
            // TODO - This won't be true for implicit method calls like this.foo()
            // TODO - Not sure if return type is resolved properly
            FunctionCall(validCandidates.head, List.empty, boundArgs, validCandidates.head.returnType)
          }
          case _ => ???
        }
      case MemberAccessExpr(base, member, range) => ???
      case blockExpr: BlockExpr => {
        stack += ListBuffer.empty
        val boundExprs = blockExpr.exprs.map(check(_))
        val inferredType = boundExprs.lastOption.map(_.semanticType).getOrElse(KahwaLangScope.UnitType)
        checkWith(inferredType)
        BoundBlockExpr(boundExprs, inferredType, semanticContext.blockToOwnScope(blockExpr), popAndGetVars())
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
          check(
            _,
            TypeConstraint.subtypeOf(
              typeRef.map(semanticContext.typeRefToSemanticType).getOrElse(KahwaLangScope.AnyType)
            )
          )
        )
        val inferredType = boundInitExpr.map(_.semanticType).getOrElse(KahwaLangScope.NothingType)

        checkWith(inferredType)

        val variableSymbol = VariableSymbol(name, semanticContext.nodeToEnclosingScope(expr))
        variableSymbol.initExpr = boundInitExpr
        variableSymbol.semanticType = inferredType

        stack.lastOption.map(_ += variableSymbol)

        if (semanticContext.nodeToEnclosingScope(expr).searchForTerm(name, current = true).nonEmpty) {
          semanticContext.diagnostics += SymbolAlreadyDeclared(name, range)
        } else {
          semanticContext.nodeToEnclosingScope(expr).define(variableSymbol)
        }

        // TODO - Do something about not being able to infer type
        BoundVariableDecl(name, inferredType, readOnly, boundInitExpr)
      }
    }
  }

  private val stack: ListBuffer[ListBuffer[VariableSymbol]] = ListBuffer.empty

  private def popAndGetVars(): ListBuffer[VariableSymbol] = {
    val res = stack.last
    stack.remove(stack.size - 1)
    res
  }

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
      if (semanticType.subtypeOf(subTypeOf) && superTypeOf.subtypeOf(semanticType)) {
        None
      } else {
//        ??? // TODO - Should have both superTypeOf and subtypeOf
        Some(TypeError(semanticType, subTypeOf, range))
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

  private def funcValid(functionSymbol: FunctionSymbol, args: List[SemanticType]): Boolean = {
    val expectedArgs = functionSymbol.parameters.map(_.semanticType)
    if (expectedArgs.size != args.size) return false
    args.zip(expectedArgs).forall { case (a, b) => a subtypeOf b }
  }
}
