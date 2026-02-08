package ast

import ast.*

class AstTransformer {
  def transform(expr: Expr): Expr = expr match {
    case BinaryExpr(expr1, expr2, op, range) =>
      BinaryExpr(transform(expr1), transform(expr2), op, range)
    case UnaryExpr(expr, op, range) => UnaryExpr(transform(expr), op, range)
    case CallExpr(callee, args, range) =>
      CallExpr(transform(callee), args.map(transform), range)
    case MemberAccessExpr(base, member, range) =>
      MemberAccessExpr(transform(base), member, range)
    case BlockExpr(exprs, range) => BlockExpr(exprs.map(transform), range)
    case IfExpr(expr, ifBlock, elseBlock, range) =>
      IfExpr(transform(expr), transform(ifBlock), elseBlock.map(transform), range)
    case WhileExpr(cond, body, range) => WhileExpr(transform(cond), transform(body), range)
    case LambdaExpr(paramList, body, range) => LambdaExpr(paramList.map(transform), transform(body), range)
    case TupleExpr(elements, range) => TupleExpr(elements.map(transform), range)
    case variableDecl: VariableDecl => variableDecl.copy(initExpr = variableDecl.initExpr.map(transform))
    case expr => expr
  }

  def transform(blockExpr: BlockExpr): BlockExpr = {
    blockExpr.copy(exprs = blockExpr.exprs.map(transform))
  }

  def transform(typeRef: TypeRef): TypeRef = {
    typeRef match {
      case AtomType(name, args, range) => AtomType(name, args.map(transform), range)
      case TupleType(elems, range) => TupleType(elems.map(transform), range)
      case FunctionType(paramList, returnType, range) =>
        FunctionType(paramList.map(transform), transform(returnType), range)
    }
  }

  def transform(variableDecl: VariableDecl): VariableDecl = {
    variableDecl.copy(initExpr = variableDecl.initExpr.map(transform))
  }

  def transform(fieldDecl: FieldDecl): FieldDecl = {
    fieldDecl.copy(initExpr = fieldDecl.initExpr.map(transform), modifiers = fieldDecl.modifiers.map(transform))
  }

  def transform(modifierNode: ModifierNode): ModifierNode = modifierNode

  def transform(typedefDecl: TypedefDecl): TypedefDecl = {
    typedefDecl.copy(
      referredType = transform(typedefDecl.referredType),
      modifiers = typedefDecl.modifiers.map(transform)
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
      typeParameters = classDecl.typeParameters.map(transform),
      nestedObjects = classDecl.nestedObjects.map(transform)
    )
  }
  
  def transform(objectDecl: ObjectDecl): ObjectDecl = {
    objectDecl.copy(
      modifiers = objectDecl.modifiers.map(transform),
      superClasses = objectDecl.superClasses.map(transform),
      fields = objectDecl.fields.map(transform),
      methods = objectDecl.methods.map(transform),
      nestedClasses = objectDecl.nestedClasses.map(transform),
      nestedObjects = objectDecl.nestedObjects.map(transform)
    )
  }

  def transform(kahwaFile: KahwaFile): KahwaFile = {
    kahwaFile.copy(
      typedefDecls = kahwaFile.typedefDecls.map(transform),
      classDecls = kahwaFile.classDecls.map(transform),
      functionDecls = kahwaFile.functionDecls.map(transform),
      variableDecls = kahwaFile.variableDecls.map(transform),
      objectDecls = kahwaFile.objectDecls.map(transform)
    )
  }

  def transform(node: AstNode): AstNode = node match {
    case expr: Expr => transform(expr)
    case typeRef: TypeRef => transform(typeRef)
    case modifierNode: ModifierNode => transform(modifierNode)
    case typedefDecl: TypedefDecl => transform(typedefDecl)
    case variableDecl: VariableDecl => transform(variableDecl)
    case fieldDecl: FieldDecl => transform(fieldDecl)
    case typeParameterDecl: TypeParameterDecl => transform(typeParameterDecl)
    case functionDecl: FunctionDecl => transform(functionDecl)
    case classDecl: ClassDecl => transform(classDecl)
    case objectDecl: ObjectDecl => transform(objectDecl)
    case kahwaFile: KahwaFile => transform(kahwaFile)
  }
}
