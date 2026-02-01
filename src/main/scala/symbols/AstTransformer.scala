package symbols

import ast.{AstNode, ClassDecl, Decl, Expr, FunctionDecl, KahwaFile, ModifierNode, Stmt, TypeParameterDecl, TypeRef, TypedefDecl, VariableDecl}

class AstTransformer {
  def transform(expr: Expr): Expr = expr
  def transform(stmt: Stmt): Stmt = stmt
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

  def transform(typeParameterDecl: TypeParameterDecl): TypeParameterDecl = typeParameterDecl

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
