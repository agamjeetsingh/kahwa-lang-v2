package ast

import ast.*

class AstTransformer {
  def transform(expr: Expr): Expr = expr match {
    case BinaryExpr(expr1, expr2, op, range) =>
      BinaryExpr(transform(expr1), transform(expr2), op, range)
    case UnaryExpr(expr, op, range) => UnaryExpr(transform(expr), op, range)
    case CallExpr(callee, args, range) =>
      CallExpr(transform(callee), args.map(transform), range)
    case IndexExpr(callee, arg, range) =>
      IndexExpr(transform(callee), transform(arg), range)
    case MemberAccessExpr(base, member, range) =>
      MemberAccessExpr(transform(base), member, range)
    case TernaryExpr(cond, expr1, expr2, range) =>
      TernaryExpr(transform(cond), transform(expr1), transform(expr2), range)
    case expr => expr
  }
  def transform(stmt: Stmt): Stmt = stmt match {
    case ExprStmt(expr, range) => ExprStmt(transform(expr), range)
    case BlockStmt(stmts, range) => BlockStmt(stmts.map(transform), range)
    case IfStmt(expr, ifBlock, elseBlock, range) =>
      IfStmt(
        transform(expr),
        transform(ifBlock),
        elseBlock.map(transform),
        range
      )
    case ReturnStmt(expr, range) => ReturnStmt(transform(expr), range)
    case WhileStmt(cond, body, range) =>
      WhileStmt(transform(cond), transform(body), range)
    case VariableDeclStmt(variableDecl, range) =>
      VariableDeclStmt(transform(variableDecl), range)
    case stmt => stmt
  }
  def transform(blockStmt: ast.BlockStmt): ast.BlockStmt = {
    blockStmt.copy(stmts = blockStmt.stmts.map(transform))
  }

  def transform(typeRef: TypeRef): TypeRef = {
    typeRef.copy(
      args = typeRef.args.map { case (argRef, variance) =>
        (transform(argRef), variance)
      }
    )
  }

  def transform(modifierNode: ModifierNode): ModifierNode = modifierNode

  def transform(typedefDecl: TypedefDecl): TypedefDecl = {
    typedefDecl.copy(
      referredType = transform(typedefDecl.referredType),
      modifiers = typedefDecl.modifiers.map(transform)
    )
  }

  def transform(variableDecl: VariableDecl): VariableDecl = {
    variableDecl.copy(
      typeRef = transform(variableDecl.typeRef),
      initExpr = variableDecl.initExpr.map(transform),
      modifiers = variableDecl.modifiers.map(transform)
    )
  }

  def transform(typeParameterDecl: TypeParameterDecl): TypeParameterDecl =
    typeParameterDecl

  def transform(functionDecl: FunctionDecl): FunctionDecl = {
    functionDecl.copy(
      returnType = transform(functionDecl.returnType),
      parameters = functionDecl.parameters.map(transform),
      block = transform(functionDecl.block),
      modifiers = functionDecl.modifiers.map(transform),
      typeParameters = functionDecl.typeParameters.map(transform)
    )
  }

  def transform(classDecl: ClassDecl): ClassDecl = {
    classDecl.copy(
      modifiers = classDecl.modifiers.map(transform),
      superClasses = classDecl.superClasses.map(transform),
      fields = classDecl.fields.map(transform),
      methods = classDecl.methods.map(transform),
      nestedClasses = classDecl.nestedClasses.map(transform),
      typeParameters = classDecl.typeParameters.map(transform)
    )
  }

  def transform(kahwaFile: KahwaFile): KahwaFile = {
    kahwaFile.copy(
      typedefDecls = kahwaFile.typedefDecls.map(transform),
      classDecls = kahwaFile.classDecls.map(transform),
      functionDecls = kahwaFile.functionDecls.map(transform),
      variableDecls = kahwaFile.variableDecls.map(transform)
    )
  }

  def transform(node: AstNode): AstNode = node match {
    case expr: Expr => transform(expr)
    case stmt: Stmt => transform(stmt)
    case typeRef: TypeRef => transform(typeRef)
    case modifierNode: ModifierNode => transform(modifierNode)
    case typedefDecl: TypedefDecl => transform(typedefDecl)
    case variableDecl: VariableDecl => transform(variableDecl)
    case typeParameterDecl: TypeParameterDecl => transform(typeParameterDecl)
    case functionDecl: FunctionDecl => transform(functionDecl)
    case classDecl: ClassDecl => transform(classDecl)
    case kahwaFile: KahwaFile => transform(kahwaFile)
  }
}
