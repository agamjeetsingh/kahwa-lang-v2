package symbols.analyser

import ast.{AtomType, FunctionType, TraversingVisitor, TupleType, TypeRef}
import symbols.{SemanticType, TypeSymbol}
import symbols.analyser.SemanticAnalyser.{MutableNodeToSymbol, MutableTypeRefToSemanticType}

import scala.collection.mutable

class TypeRefQualifier(
    val nodeToScope: NodeToScope,
    val nodeToSymbol: MutableNodeToSymbol
) extends TraversingVisitor[MutableTypeRefToSemanticType] {
  override protected def defaultResult: MutableTypeRefToSemanticType =
    mutable.Map.empty

  override protected def combine(
      r1: MutableTypeRefToSemanticType,
      r2: MutableTypeRefToSemanticType
  ): MutableTypeRefToSemanticType = r1 ++ r2

  override def visitTypeRef(node: TypeRef): MutableTypeRefToSemanticType = {
    resolveSemanticType(node)._2
  }

  extension (pair: (SemanticType, MutableTypeRefToSemanticType)) {
    private def ~>(map: MutableTypeRefToSemanticType): SemanticType = {
      map ++= pair._2
      pair._1
    }
  }

  private def resolveSemanticType(node: TypeRef): (SemanticType, MutableTypeRefToSemanticType) = {
    val map: MutableTypeRefToSemanticType = mutable.Map.empty
    val semanticType = node match {
      case atomicType: AtomType =>
        SemanticType(typeRefToSymbol(atomicType), atomicType.args.map(t => resolveSemanticType(t) ~> map))
      case TupleType(elems, _) =>
        SemanticType(KahwaLangScope.tupleSymbols(elems.size), elems.map(t => resolveSemanticType(t) ~> map))
      case FunctionType(paramList, returnType, _) =>
        SemanticType(
          KahwaLangScope.functionSymbols(paramList.size),
          paramList.map(t => resolveSemanticType(t) ~> map) ++ List(resolveSemanticType(returnType) ~> map)
        )
    }
    map += node -> semanticType
    (semanticType, map)
  }

  private def typeRefToSymbol(node: AtomType): TypeSymbol = {
    nodeToScope(node.name)
      .searchForType(node.name)
      .getOrElse(KahwaLangScope.ErrorTypeSymbol)
  }
}
