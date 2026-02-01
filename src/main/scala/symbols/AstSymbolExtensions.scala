package symbols

import ast.{AstNode, ClassDecl, FunctionDecl, KahwaFile, TypeParameterDecl, TypedefDecl, VariableDecl}

/**
 * Type-safe extension methods for retrieving symbols from AST nodes.
 *
 * These methods encapsulate the pattern matching and casting needed to retrieve
 * specific symbol types from the nodeToSymbol map. They return non-Option types
 * because after the DeclareNames phase, all declarations should have corresponding symbols.
 *
 * If a symbol is not found, these methods will throw IllegalStateException with
 * a descriptive error message, indicating that the DeclareNames phase may not have completed.
 */
object AstSymbolExtensions {

  extension (classDecl: ClassDecl) {
    /**
     * Retrieves the ClassSymbol associated with this ClassDecl.
     * @throws IllegalStateException if the symbol is not found or is not a ClassSymbol
     */
    def symbol(using map: Map[AstNode, Symbol]): ClassSymbol =
      map.get(classDecl).collect { case s: ClassSymbol => s }
        .getOrElse(throw IllegalStateException(
          s"ClassSymbol not found for class '${classDecl.name}' at ${classDecl.range}. " +
          s"Ensure DeclareNames phase has completed."
        ))

    /**
     * Retrieves the Scope associated with this ClassDecl's symbol.
     */
    def symbolScope(using map: Map[AstNode, Symbol]): Scope = symbol.scope
  }

  extension (functionDecl: FunctionDecl) {
    /**
     * Retrieves the FunctionSymbol (or MethodSymbol) associated with this FunctionDecl.
     * Note: MethodSymbol extends FunctionSymbol, so this covers both cases.
     * @throws IllegalStateException if the symbol is not found or is not a FunctionSymbol
     */
    def symbol(using map: Map[AstNode, Symbol]): FunctionSymbol =
      map.get(functionDecl).collect { case s: FunctionSymbol => s }
        .getOrElse(throw IllegalStateException(
          s"FunctionSymbol not found for function '${functionDecl.name}' at ${functionDecl.range}. " +
          s"Ensure DeclareNames phase has completed."
        ))

    /**
     * Retrieves the Scope associated with this FunctionDecl's symbol.
     */
    def symbolScope(using map: Map[AstNode, Symbol]): Scope = symbol.scope

    /**
     * Checks if this FunctionDecl is associated with a MethodSymbol (vs a top-level FunctionSymbol).
     */
    def isMethod(using map: Map[AstNode, Symbol]): Boolean = symbol.isInstanceOf[MethodSymbol]

    /**
     * Retrieves the MethodSymbol if this is a method, or None if it's a top-level function.
     */
    def asMethodSymbol(using map: Map[AstNode, Symbol]): Option[MethodSymbol] =
      map.get(functionDecl).collect { case s: MethodSymbol => s }
  }

  extension (typedefDecl: TypedefDecl) {
    /**
     * Retrieves the TypedefSymbol associated with this TypedefDecl.
     * @throws IllegalStateException if the symbol is not found or is not a TypedefSymbol
     */
    def symbol(using map: Map[AstNode, Symbol]): TypedefSymbol =
      map.get(typedefDecl).collect { case s: TypedefSymbol => s }
        .getOrElse(throw IllegalStateException(
          s"TypedefSymbol not found for typedef '${typedefDecl.name}' at ${typedefDecl.range}. " +
          s"Ensure DeclareNames phase has completed."
        ))

    /**
     * Retrieves the Scope associated with this TypedefDecl's symbol.
     */
    def symbolScope(using map: Map[AstNode, Symbol]): Scope = symbol.scope
  }

  extension (variableDecl: VariableDecl) {
    /**
     * Retrieves the VariableSymbol associated with this VariableDecl.
     * Note: This returns VariableSymbol which is the base type for VisibleVariableSymbol
     * and FieldSymbol, so it covers all three cases.
     * @throws IllegalStateException if the symbol is not found or is not a VariableSymbol
     */
    def symbol(using map: Map[AstNode, Symbol]): VariableSymbol =
      map.get(variableDecl).collect { case s: VariableSymbol => s }
        .getOrElse(throw IllegalStateException(
          s"VariableSymbol not found for variable '${variableDecl.name}' at ${variableDecl.range}. " +
          s"Ensure DeclareNames phase has completed."
        ))

    /**
     * Retrieves the Scope associated with this VariableDecl's symbol.
     */
    def symbolScope(using map: Map[AstNode, Symbol]): Scope = symbol.scope

    /**
     * Checks if this VariableDecl is associated with a FieldSymbol.
     */
    def isField(using map: Map[AstNode, Symbol]): Boolean = symbol.isInstanceOf[FieldSymbol]

    /**
     * Checks if this VariableDecl is associated with a VisibleVariableSymbol (top-level or field).
     */
    def isVisible(using map: Map[AstNode, Symbol]): Boolean = symbol.isInstanceOf[VisibleVariableSymbol]

    /**
     * Retrieves the FieldSymbol if this is a field, or None otherwise.
     */
    def asFieldSymbol(using map: Map[AstNode, Symbol]): Option[FieldSymbol] =
      map.get(variableDecl).collect { case s: FieldSymbol => s }

    /**
     * Retrieves the VisibleVariableSymbol if this is a visible variable, or None otherwise.
     */
    def asVisibleVariableSymbol(using map: Map[AstNode, Symbol]): Option[VisibleVariableSymbol] =
      map.get(variableDecl).collect { case s: VisibleVariableSymbol => s }
  }

  extension (typeParameterDecl: TypeParameterDecl) {
    /**
     * Retrieves the TypeParameterSymbol associated with this TypeParameterDecl.
     * @throws IllegalStateException if the symbol is not found or is not a TypeParameterSymbol
     */
    def symbol(using map: Map[AstNode, Symbol]): TypeParameterSymbol =
      map.get(typeParameterDecl).collect { case s: TypeParameterSymbol => s }
        .getOrElse(throw IllegalStateException(
          s"TypeParameterSymbol not found for type parameter '${typeParameterDecl.name}' at ${typeParameterDecl.range}. " +
          s"Ensure DeclareNames phase has completed."
        ))

    /**
     * Retrieves the Scope associated with this TypeParameterDecl's symbol.
     */
    def symbolScope(using map: Map[AstNode, Symbol]): Scope = symbol.scope
  }

  extension (kahwaFile: KahwaFile) {
    /**
     * Retrieves the TranslationUnit associated with this KahwaFile.
     * @throws IllegalStateException if the symbol is not found or is not a TranslationUnit
     */
    def symbol(using map: Map[AstNode, Symbol]): TranslationUnit =
      map.get(kahwaFile).collect { case s: TranslationUnit => s }
        .getOrElse(throw IllegalStateException(
          s"TranslationUnit not found for file at ${kahwaFile.range}. " +
          s"Ensure DeclareNames phase has completed."
        ))

    /**
     * Retrieves the Scope associated with this KahwaFile's TranslationUnit.
     */
    def symbolScope(using map: Map[AstNode, Symbol]): Scope = symbol.scope
  }

  /**
   * Generic extension for any AstNode that might have a symbol.
   * This is useful when you don't know the specific type of the node.
   */
  extension (node: AstNode) {
    /**
     * Retrieves the Symbol associated with this AstNode, if any.
     * Returns None if no symbol is associated with this node.
     */
    def symbolOpt(using map: Map[AstNode, Symbol]): Option[Symbol] =
      map.get(node)

    /**
     * Retrieves the Scope associated with this AstNode's symbol.
     * Returns an empty Scope if no symbol is found.
     */
    def symbolScope(using map: Map[AstNode, Symbol]): Scope =
      symbolOpt.map(_.scope).getOrElse(Scope())
  }
}