package symbols.analyser

import ast.{AtomType, ClassDecl, FunctionDecl, FunctionType, ObjectDecl, TraversingVisitor, TupleType, TypeParameterDecl, TypeRef, TypedefDecl, VariableDecl}
import symbols.{ClassSymbol, FunctionSymbol, ObjectSymbol, SemanticType, TypeParameterSymbol, TypeSymbol, TypedefSymbol, VariableSymbol}
import symbols.analyser.SemanticAnalyser.SemanticContext

class TypeRefQualifier(
    val semanticContext: SemanticContext
) extends TraversingVisitor[Unit] {
  override protected def defaultResult: Unit = {}

  override def visitFunctionDecl(node: FunctionDecl): Unit = {
    super.visitFunctionDecl(node)
    semanticContext.nodeToSymbol.get(node).collect { case functionSymbol: FunctionSymbol => functionSymbol }.foreach {
      _.returnType = semanticContext.typeRefToSemanticType(node.returnType)
    }
  }

  override def visitClassDecl(node: ClassDecl): Unit = {
    super.visitClassDecl(node)
    semanticContext.nodeToSymbol.get(node).collect { case classSymbol: ClassSymbol => classSymbol }.foreach {
      _.superClasses ++= node.superClasses.map(semanticContext.typeRefToSemanticType)
    }
  }

  override def visitObjectDecl(node: ObjectDecl): Unit = {
    super.visitObjectDecl(node)
    semanticContext.nodeToSymbol.get(node).collect { case objectSymbol: ObjectSymbol => objectSymbol }.foreach {
      _.superClasses ++= node.superClasses.map(semanticContext.typeRefToSemanticType)
    }
  }

  override def visitVariableDecl(node: VariableDecl): Unit = {
    super.visitVariableDecl(node)
    semanticContext.nodeToSymbol.get(node).collect { case variableSymbol: VariableSymbol => variableSymbol }.foreach {
      // TODO - Find out which variable decls can have no type
      // TODO - Enforce that non local variables have a type
      //      - Or maybe leave it to type inference
      variableSymbol => variableSymbol.semanticType = semanticContext.typeRefToSemanticType(node.typeRef.get)
    }
  }

  override def visitTypedefDecl(node: TypedefDecl): Unit = {
    super.visitTypedefDecl(node)
    semanticContext.nodeToSymbol.get(node).collect { case typedefSymbol: TypedefSymbol => typedefSymbol }.foreach {
      typedefSymbol => typedefSymbol.referredType = semanticContext.typeRefToSemanticType(node.referredType)
    }
  }

  override def visitTypeParameterDecl(node: TypeParameterDecl): Unit = {
    super.visitTypeParameterDecl(node)
    semanticContext.nodeToSymbol.get(node).collect { case typeParameterSymbol: TypeParameterSymbol => typeParameterSymbol }.foreach {
      typeParameterSymbol => 
        typeParameterSymbol.lowerBounds ++= node.lowerBounds.map(semanticContext.typeRefToSemanticType)
        typeParameterSymbol.upperBounds ++= node.upperBounds.map(semanticContext.typeRefToSemanticType)
    }
  }

  override def visitTypeRef(node: TypeRef): Unit = resolveSemanticType(node)

  private def resolveSemanticType(node: TypeRef): SemanticType = {
    val semanticType = node match {
      case atomicType: AtomType =>
        SemanticType(typeRefToSymbol(atomicType), atomicType.args.map(t => resolveSemanticType(t)))
      case TupleType(elems, _) =>
        SemanticType(KahwaLangScope.tupleSymbols(elems.size), elems.map(t => resolveSemanticType(t)))
      case FunctionType(paramList, returnType, _) =>
        SemanticType(
          KahwaLangScope.functionSymbols(paramList.size),
          paramList.map(t => resolveSemanticType(t)) ++ List(resolveSemanticType(returnType))
        )
    }
    semanticContext.typeRefToSemanticType += node -> semanticType
    semanticType
  }

  private def typeRefToSymbol(node: AtomType): TypeSymbol = {
    semanticContext
      .nodeToEnclosingScope(node.name)
      .searchForType(node.name)
      .getOrElse(KahwaLangScope.getTypeSymbol(KahwaLangScope.ErrorType))
  }
}
