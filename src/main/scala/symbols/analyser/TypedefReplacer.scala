package symbols.analyser

import ast.{AstTransformer, TypeRef, TypedefDecl}
import symbols.TypeSymbol
import symbols.analyser.SemanticAnalyser.MutableTypeRefToSemanticType

private class TypedefReplacer(allTypedefs: List[TypedefDecl], typeRefToSymbol: MutableTypeRefToSemanticType) extends AstTransformer {
  override def transform(typeRef: TypeRef): TypeRef = {
    typeRefToSymbol.get(typeRef) match {
      case Some(semanticType) => ???
        // Found a typedef! Replace it and recurse (for typedef chains)
//        transform(targetType.copy(range = typeRef.range))
      case None =>
        // Not a typedef, but still recurse into generic arguments
        super.transform(typeRef)
    }
  }
}
