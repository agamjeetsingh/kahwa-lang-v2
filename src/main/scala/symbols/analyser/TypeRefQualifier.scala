//package symbols.analyser
//
//import ast.{Ident, TraversingVisitor, TypeRef}
//import symbols.{GlobalScope, SemanticType, TypeSymbol}
//import symbols.analyser.SemanticAnalyser.{MutableIdentToSymbol, MutableNodeToSymbol, MutableTypeRefToSemanticType}
//
//import scala.collection.mutable
//
//class TypeRefQualifier(
//    val nodeToScope: NodeToScope,
//    val nodeToSymbol: MutableNodeToSymbol
//) extends TraversingVisitor[MutableTypeRefToSemanticType] {
//  override protected def defaultResult: MutableTypeRefToSemanticType =
//    mutable.Map.empty
//
//  override protected def combine(
//      r1: MutableTypeRefToSemanticType,
//      r2: MutableTypeRefToSemanticType
//  ): MutableTypeRefToSemanticType = r1 ++ r2
//
//  override def visitTypeRef(node: TypeRef): MutableTypeRefToSemanticType = {
//    super.visitTypeRef(node) ++ resolveIdentType(node)
//  }
//
//  private def resolveIdentType(node: TypeRef): MutableTypeRefToSemanticType = {
//    mutable.Map(
//      node -> SemanticType(
//        typeRefToSymbol(node),
//        node.args
//          .map(_._1)
//          .map(t => SemanticType(typeRefToSymbol(t), List.empty))
//      )
//    )
//  }
//
//  private def typeRefToSymbol(node: TypeRef): TypeSymbol = {
//    nodeToScope(node.name)
//      .searchForType(node.name)
//      .headOption
//      .getOrElse(GlobalScope.ErrorTypeSymbol)
//  }
//}
