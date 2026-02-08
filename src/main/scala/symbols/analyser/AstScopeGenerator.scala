package symbols.analyser

import ast.{
  AstNode,
  BinaryExpr,
  BlockExpr,
  BreakExpr,
  CallExpr,
  ClassDecl,
  ContinueExpr,
  Decl,
  Expr,
  FunctionDecl,
  Ident,
  IfExpr,
  KahwaFile,
  LambdaExpr,
  LiteralExpr,
  MemberAccessExpr,
  TraversingVisitor,
  TupleExpr,
  TypeRef,
  TypedefDecl,
  UnaryExpr,
  VariableDecl,
  WhileExpr
}
import symbols.Scope
import symbols.analyser.SemanticAnalyser.MutableNodeToScope

import scala.collection.mutable

class AstScopeGenerator(val nodeToSymbol: NodeToSymbol) extends TraversingVisitor[MutableNodeToScope] {

  override protected def defaultResult: MutableNodeToScope = mutable.Map.empty

  override protected def combine(
      r1: MutableNodeToScope,
      r2: MutableNodeToScope
  ): MutableNodeToScope = r1 ++ r2

  // KahwaFile is the root and creates a scope from its symbol
  override def visitKahwaFile(node: KahwaFile): MutableNodeToScope =
    withScopeFrom(node, super.visitKahwaFile)

  // All declarations create scopes from their symbols
  override def visitClassDecl(node: ClassDecl): MutableNodeToScope =
    withScopeFrom(node, super.visitClassDecl)

  override def visitFunctionDecl(node: FunctionDecl): MutableNodeToScope =
    withScopeFrom(node, super.visitFunctionDecl)

  override def visitVariableDecl(node: VariableDecl): MutableNodeToScope =
    withScopeFrom(node, super.visitVariableDecl)

  override def visitTypedefDecl(node: TypedefDecl): MutableNodeToScope =
    withScopeFrom(node, super.visitTypedefDecl)

  // All expressions just need to be mapped to the current scope
  override def visitExpr(node: Expr): MutableNodeToScope = {
    node match {
      case block: BlockExpr => withScopeFromBlock(block, super.visitExpr)
      case _ => addAndRecurse(node, super.visitExpr)
    }
  }

  override def visitTypeRef(node: TypeRef): MutableNodeToScope =
    addAndRecurse(node, super.visitTypeRef)

  private def addAndRecurse[T <: AstNode](
      node: T,
      recurse: T => MutableNodeToScope
  ): MutableNodeToScope = {
    recurse(node) ++ mutable.Map(node -> stack.top)
  }

  private def withScopeFrom[T <: Decl](
      node: T,
      recurse: T => MutableNodeToScope
  ): MutableNodeToScope = {
    stack.push(nodeToSymbol(node).scope)
    try {
      recurse(node) ++ mutable.Map(node -> nodeToSymbol(node).scope)
    } finally {
      stack.pop()
    }
  }

  private def withScopeFromBlock(node: BlockExpr, recurse: Expr => MutableNodeToScope): MutableNodeToScope = {
    stack.push(node.scope)
    try {
      recurse(node) ++ mutable.Map(node -> node.scope)
    } finally {
      stack.pop()
    }
  }

  private val stack: mutable.Stack[Scope] = mutable.Stack()
}
