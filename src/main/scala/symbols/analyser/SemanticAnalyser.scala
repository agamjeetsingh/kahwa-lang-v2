package symbols.analyser

import ast.*
import diagnostics.Diagnostic
import symbols.{Scope, Symbol, TranslationUnit}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

type NodeToSymbol = Map[Decl, Symbol]
type IdentToSymbol = Map[Ident, Symbol]
type NodeToScope = Map[AstNode, Scope]

object SemanticAnalyser {
  private[analyser] type MutableNodeToSymbol = mutable.Map[Decl, Symbol]
  private type MutableIdentToSymbol = mutable.Map[Ident, Symbol]
  private[analyser] type MutableNodeToScope = mutable.Map[AstNode, Scope]
  def processFile(file: KahwaFile): (TranslationUnit, List[Diagnostic]) = {
    var kahwaFile = file

    // Phase 1: Compress member access expressions to unqual idents ((a.b).c) -> (a.b.c)
    kahwaFile = AccessCompressor.transform(kahwaFile)

    given nodeToSymbol: MutableNodeToSymbol = mutable.Map()
    given diagnostics: ListBuffer[Diagnostic] = ListBuffer()

    // Phase 2: Declare all top-level functions, top-level variables, classes, fields, methods and function/method parameters
    val res = DeclareNames.declareFile(kahwaFile)

    // Phase 3: Provide a scope to every single AST Node
    val nodeToScope: MutableNodeToScope = AstScopeGenerator(nodeToSymbol.toMap).visitKahwaFile(kahwaFile)

    // Phase 4: Build a map from Idents to Symbols (TODO)
    val identToSymbol: MutableIdentToSymbol = mutable.Map.empty

    // Phase 5: Detect cycles in the typedefs (TODO)
    diagnostics ++= TypedefCycleDetector.detectCycles(kahwaFile.typedefDecls)

    // Phase 6: Replace each type def with the right type (TODO)
//    kahwaFile = TypedefReplacer(res.typedefs.toList).transform(kahwaFile)

    // Phase 7: Resolve all typeRefs to semantic types except for the ones in method bodies
    // TODO - Can be simplified a lot by using the nodeToScope map
    diagnostics ++= PartialTypeResolver.TypeResolver(nodeToSymbol.toMap, nodeToScope.toMap).visitKahwaFile(kahwaFile)

    (res, diagnostics.toList)
  }

  private class QualifyIdents(nodeToScope: NodeToScope) extends AstTransformer {
    given NodeToScope = nodeToScope
    override def transform(typeRef: TypeRef): TypeRef = {
      ???
//      nodeToScope(typeRef).searchForType(typeRef.name).headOption match {
//        case Some(typeSymbol) => TypeRef(
//          Qual(typeRef.name.asInstanceOf[Unqual].name, typeSymbol, typeRef.name.range),
//          typeRef.args.map((typeRef, variance) => (transform(typeRef), variance)),
//          typeRef.range)
//        case None => super.transform(typeRef)
//      }
    }

    override def transform(expr: Expr): Expr = {
      expr match {
        case ident: Ident => ???
        case BinaryExpr(expr1, expr2, op, range) => ???
        case UnaryExpr(expr, op, range) => ???
        case CallExpr(callee, args, range) => ???
        case IndexExpr(callee, arg, range) => ???
        case MemberAccessExpr(base, member, range) => ???
        case _ => super.transform(expr)
      }
    }
  }
}
