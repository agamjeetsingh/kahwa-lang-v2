package symbols.analyser

import ast.{AtomType, FunctionType, TraversingVisitor, TupleType, TypeRef}
import symbols.{SemanticType, TypeSymbol}
import symbols.analyser.SemanticAnalyser.{MutableNodeToSymbol, MutableTypeRefToSemanticType, SemanticContext}

import scala.collection.mutable

class TypeRefQualifier(
    val semanticContext: SemanticContext
) extends TraversingVisitor[Unit] {
  override protected def defaultResult: Unit = {}

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
    semanticContext.nodeToScope(node.name)
      .searchForType(node.name)
      .getOrElse(KahwaLangScope.ErrorTypeSymbol)
  }
}
