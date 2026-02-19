package symbols.analyser

import ast.*
import diagnostics.Diagnostic
import symbols.analyser.SemanticAnalyser.{MutableNodeToSymbol, MutableTypeRefToSemanticType, SemanticContext}
import symbols.*

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

type NodeToSymbol = Map[Decl, Symbol]
type IdentToSymbol = Map[Ident, Symbol]
type NodeToScope = Map[AstNode, Scope]

object SemanticAnalyser {
  private[analyser] type MutableNodeToSymbol = mutable.Map[Decl, Symbol]
  private[analyser] type MutableNodeToScope = mutable.Map[AstNode, Scope]
  private[analyser] type MutableTypeRefToSemanticType = mutable.Map[TypeRef, SemanticType]
  private[analyser] type MutableBlockToOwnScope = mutable.Map[BlockExpr, Scope]

  private[analyser] class SemanticContext {
    /**
     * All phases of semantic analysis add to diagnostics
     */
    val diagnostics: ListBuffer[Diagnostic] = ListBuffer()
    /**
     * Initialised completely by [[DeclareNames]]
     *
     * The following types of nodes have a symbol associated with them:
     *  - [[KahwaFile]] -> [[TranslationUnit]]
     *  - [[TypeParameterDecl]] -> [[TypeParameterSymbol]]
     *  - [[ClassDecl]] -> [[ClassSymbol]]
     *  - [[ObjectDecl]] -> [[ObjectSymbol]]
     *  - [[VariableDecl]] -> [[VariableSymbol]] | [[VisibleVariableSymbol]] | [[FieldSymbol]]
     *  - [[FunctionDecl]] -> [[FunctionSymbol]] | [[MethodSymbol]]
     *  - [[TypedefDecl]] -> [[TypedefSymbol]]
     */
    val nodeToSymbol: MutableNodeToSymbol = mutable.Map.empty
    /**
     * Initialised completely by [[AstScopeGenerator]]
     */
    val nodeToEnclosingScope: MutableNodeToScope = mutable.Map.empty
    /**
     * Initialised completely by [[AstScopeGenerator]]
     */
    val blockToOwnScope: MutableBlockToOwnScope = mutable.Map.empty
    /**
     * Initialised mostly by [[TypeRefQualifier]], the exception being local variables
     *
     * Used by [[TypeChecker]] for local variables that have types mentioned
     */
    val typeRefToSemanticType: MutableTypeRefToSemanticType = mutable.Map.empty
  }

  def processFile(file: KahwaFile): (TranslationUnit, List[Diagnostic]) = {
    var kahwaFile = file

    // Phase 1: Compress member access expressions to idents ((a.b).c) -> (a.b.c)
    kahwaFile = AccessCompressor.transform(kahwaFile)

    val semanticContext = SemanticContext()

    // Phase 2: Declare all top-level functions, top-level variables, classes, fields, methods and function/method parameters
    // TODO - Linking of classes and objects
    val translationUnit = DeclareNames(semanticContext).declareFile(kahwaFile)

    // Phase 3: Provide a scope to every single AST Node
    AstScopeGenerator(semanticContext).visitKahwaFile(kahwaFile)

    // Phase 4: Build a map from TypeRefs to Semantic Types
    TypeRefQualifier(semanticContext).visitKahwaFile(kahwaFile)

    // Phase 5: Detect cycles in the typedefs
//    diagnostics ++= TypedefCycleDetector.detectCycles(kahwaFile.typedefDecls)

    // Phase 6: Replace each type def with the right type (TODO - Repair nodeToScope)
//    kahwaFile = TypedefReplacer(kahwaFile.typedefDecls, typeRefToSemanticType).transform(kahwaFile)

    // Phase 7:

    val boundExprs = TypeCheck(semanticContext).visitKahwaFile(kahwaFile)
    println(boundExprs.mkString("\n"))

    (translationUnit, semanticContext.diagnostics.toList)
  }
}

class TypeCheck(
    val semanticContext: SemanticContext
) extends TraversingVisitor[List[BoundExpr]] {

  override protected def defaultResult: List[BoundExpr] = List.empty

  override protected def combine(r1: List[BoundExpr], r2: List[BoundExpr]): List[BoundExpr] = r1 ++ r2

  override def visitExpr(node: Expr): List[BoundExpr] = {
    List(TypeChecker(semanticContext).check(node))
  }
}
