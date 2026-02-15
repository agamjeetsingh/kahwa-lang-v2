package symbols.analyser

import ast.{AstNode, BinaryExpr, BlockExpr, BreakExpr, CallExpr, ClassDecl, ContinueExpr, Decl, Expr, FunctionDecl, Ident, IfExpr, KahwaFile, LambdaExpr, LiteralExpr, MemberAccessExpr, TraversingVisitor, TupleExpr, TypeRef, TypedefDecl, UnaryExpr, VariableDecl, WhileExpr}
import symbols.Scope
import symbols.analyser.SemanticAnalyser.{MutableBlockToOwnScope, MutableNodeToScope, SemanticContext}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class AstScopeGenerator(
    val semanticContext: SemanticContext
) extends TraversingVisitor[Unit] {

  override protected def defaultResult: Unit = {}

  // KahwaFile is the root and creates a scope from its symbol
  override def visitKahwaFile(node: KahwaFile): Unit =
    withScopeFrom(node, super.visitKahwaFile)

  // All declarations create scopes from their symbols
  override def visitClassDecl(node: ClassDecl): Unit =
    withScopeFrom(node, super.visitClassDecl)

  override def visitFunctionDecl(node: FunctionDecl): Unit =
    withScopeFrom(node, super.visitFunctionDecl)

  override def visitTypedefDecl(node: TypedefDecl): Unit =
    withScopeFrom(node, super.visitTypedefDecl)

  // All expressions just need to be mapped to the current scope
  override def visitExpr(node: Expr): Unit = {
    node match {
      case block: BlockExpr => withScopeFromBlock(block, super.visitExpr)
      case _ => addAndRecurse(node, super.visitExpr)
    }
  }

  override def visitVariableDecl(node: VariableDecl): Unit = {
    addAndRecurse(node, super.visitVariableDecl)
  }

  override def visitIdent(node: Ident): Unit = {
    addAndRecurse(node, super.visitIdent)
  }

  override def visitTypeRef(node: TypeRef): Unit =
    addAndRecurse(node, super.visitTypeRef)

  private def addAndRecurse[T <: AstNode](
      node: T,
      recurse: T => Unit
  ): Unit = {

    recurse(node)
    semanticContext.nodeToScope += node -> stack.last
  }

  private def withScopeFrom[T <: Decl](
      node: T,
      recurse: T => Unit
  ): Unit = {
    stack += semanticContext.nodeToSymbol.get(node).map(_.scope).getOrElse(Scope())
    recurse(node)
    stack.remove(stack.length - 1)
    semanticContext.nodeToScope ++= stack.lastOption.map(node -> _)
  }

  private def withScopeFromBlock(node: BlockExpr, recurse: BlockExpr => Unit): Unit = {
    val newScope = Scope()
    stack.lastOption.foreach(newScope.addOuterScope)
    semanticContext.blockToOwnScope += node -> newScope
    stack += newScope
    recurse(node)
    stack.remove(stack.length - 1)
    semanticContext.nodeToScope ++= stack.lastOption.map(node -> _)
  }

  private val stack: mutable.ListBuffer[Scope] = ListBuffer()
}
