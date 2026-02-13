package symbols.analyser

import ast.*
import diagnostics.Diagnostic
import symbols.{Scope, SemanticType, Symbol, TranslationUnit, TypeSymbol}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

type NodeToSymbol = Map[Decl, Symbol]
type IdentToSymbol = Map[Ident, Symbol]
type NodeToScope = Map[AstNode, Scope]

object SemanticAnalyser {
  private[analyser] type MutableNodeToSymbol = mutable.Map[Decl, Symbol]
  private[analyser] type MutableIdentToSymbol = mutable.Map[Ident, Symbol]
  private[analyser] type MutableNodeToScope = mutable.Map[AstNode, Scope]
  private[analyser] type MutableTypeRefToSemanticType = mutable.Map[TypeRef, SemanticType]
  private[analyser] type MutableExprToType = mutable.Map[Expr, SemanticType]
  def processFile(file: KahwaFile): (TranslationUnit, List[Diagnostic]) = {
    var kahwaFile = file

    // Phase 1: Compress member access expressions to idents ((a.b).c) -> (a.b.c)
    kahwaFile = AccessCompressor.transform(kahwaFile)
    val nodeToSymbol: MutableNodeToSymbol = mutable.Map()
    given MutableNodeToSymbol = nodeToSymbol
    val diagnostics: ListBuffer[Diagnostic] = ListBuffer()
    given ListBuffer[Diagnostic] = diagnostics

    // Phase 2: Declare all top-level functions, top-level variables, classes, fields, methods and function/method parameters
    // TODO - Linking of classes and objects
    val res = DeclareNames.declareFile(kahwaFile)

    // Phase 3: Provide a scope to every single AST Node
    val nodeToScope: MutableNodeToScope =
      AstScopeGenerator(nodeToSymbol.toMap).visitKahwaFile(kahwaFile)

    // Phase 4: Build a map from TypeRefs to Semantic Types
    val typeRefToSemanticType: MutableTypeRefToSemanticType =
      TypeRefQualifier(nodeToScope.toMap, nodeToSymbol).visitKahwaFile(kahwaFile)

    // Phase 5: Detect cycles in the typedefs
//    diagnostics ++= TypedefCycleDetector.detectCycles(kahwaFile.typedefDecls)

    // Phase 6: Replace each type def with the right type (TODO - Repair nodeToScope)
    kahwaFile = TypedefReplacer(kahwaFile.typedefDecls, typeRefToSemanticType).transform(kahwaFile)

    // Phase 7:
    
    TypeChecker(nodeToSymbol, nodeToScope.toMap, typeRefToSemanticType, diagnostics)

    (res, diagnostics.toList)
  }
}
