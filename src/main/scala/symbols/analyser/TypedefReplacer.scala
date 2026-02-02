package symbols.analyser

import ast.{AstTransformer, TypeRef}

private class TypedefReplacer(typedefMap: Map[String, TypeRef]) extends AstTransformer {
  override def transform(typeRef: TypeRef): TypeRef = {
    // Look up the name in the typedef map
    // TODO
    typedefMap.get(typeRef.name.head) match {
      case Some(targetType) =>
        // Found a typedef! Replace it and recurse (for typedef chains)
        transform(targetType.copy(range = typeRef.range))
      case None =>
        // Not a typedef, but still recurse into generic arguments
        super.transform(typeRef)
    }
  }
}
