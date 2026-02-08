//package symbols.analyser
//
//import ast.{AstNode, ClassDecl, FunctionDecl, KahwaFile, TraversingVisitor, TypeRef, TypedefDecl, VariableDecl}
//import diagnostics.Diagnostic
//import diagnostics.Diagnostic.{CannotResolveSymbol, IncorrectNumberOfGenericArguments}
//import symbols.AstSymbolExtensions.symbol
//import symbols.{ClassSymbol, GlobalScope, Scope, SemanticType}
//
//import scala.collection.mutable
//import scala.collection.mutable.ListBuffer
//
//private object PartialTypeResolver {
//  extension [T](symbolAndDiagnostics: (T, Iterable[Diagnostic])) {
//    private def ~>(sink: ListBuffer[Diagnostic]): T = {
//      sink ++= symbolAndDiagnostics._2
//      symbolAndDiagnostics._1
//    }
//  }
//
//  class TypeResolver(
//      val nodeToSymbol: NodeToSymbol,
//      val nodeToScope: NodeToScope
//  ) extends TraversingVisitor[ListBuffer[Diagnostic]] {
//    given NodeToSymbol = nodeToSymbol
//    override protected def defaultResult: ListBuffer[Diagnostic] = ListBuffer()
//
//    override protected def combine(
//        r1: ListBuffer[Diagnostic],
//        r2: ListBuffer[Diagnostic]
//    ): ListBuffer[Diagnostic] = r1 ++ r2
//
//    override def visitKahwaFile(node: KahwaFile): ListBuffer[Diagnostic] = {
//      withScope(node) {
//        super.visitKahwaFile(node)
//      }
//    }
//
//    override def visitTypedefDecl(node: TypedefDecl): ListBuffer[Diagnostic] = {
//      val (semanticType, diagnostics) =
//        resolveAndRecurse(node.referredType, super.visitTypedefDecl)(node)
//      node.symbol.referredType = semanticType
//      diagnostics
//    }
//
//    override def visitClassDecl(node: ClassDecl): ListBuffer[Diagnostic] = {
//      resolveAndRecurse(
//        node.symbol.superClasses,
//        node.superClasses,
//        super.visitClassDecl
//      )(node)
//    }
//
//    override def visitFunctionDecl(
//        node: FunctionDecl
//    ): ListBuffer[Diagnostic] = {
//      val (semanticType, diagnostics) =
//        resolveAndRecurse(node.returnType, super.visitFunctionDecl)(node)
//      node.symbol.returnType = semanticType
//      diagnostics
//    }
//
//    override def visitVariableDecl(
//        node: VariableDecl
//    ): ListBuffer[Diagnostic] = {
//      val (semanticType, diagnostics) =
//        resolveAndRecurse(node.typeRef, super.visitVariableDecl)(node)
//      node.symbol.semanticType = semanticType
//      diagnostics
//    }
//
//    private def resolveAndRecurse[T <: AstNode](
//        semanticTypes: ListBuffer[SemanticType],
//        typeRefs: List[TypeRef],
//        recursiveVisitor: T => ListBuffer[Diagnostic]
//    )(node: T) = {
//      val diagnostics = ListBuffer[Diagnostic]()
//      semanticTypes ++= typeRefs.map(resolveType(_, stack.top) ~> diagnostics)
//
//      withScope(node) {
//        diagnostics ++ recursiveVisitor(node)
//      }
//    }
//
//    private def resolveAndRecurse[T <: AstNode](
//        typeRef: TypeRef,
//        recursiveVisitor: T => ListBuffer[Diagnostic]
//    )(node: T): (SemanticType, ListBuffer[Diagnostic]) = {
//      val diagnostics = ListBuffer[Diagnostic]()
//      val res = resolveType(typeRef, stack.top) ~> diagnostics
//
//      withScope(node) {
//        (res, diagnostics ++ recursiveVisitor(node))
//      }
//    }
//
//    private def withScope[R](astNode: AstNode)(body: => R): R = {
//      stack.push(nodeToScope(astNode))
//      try {
//        body
//      } finally {
//        stack.pop()
//      }
//    }
//
//    private val stack: mutable.Stack[Scope] = mutable.Stack()
//  }
//
//  private def resolveType(
//      typeRef: TypeRef,
//      scope: Scope
//  ): (SemanticType, ListBuffer[Diagnostic]) = {
//    val symbols = scope.searchForType(typeRef.name.prettyPrint)
//    val diagnostics: ListBuffer[Diagnostic] = ListBuffer()
//    symbols.headOption match {
//      case Some(symbol) => {
//        val expectedArgs = symbol match {
//          case classSymbol: ClassSymbol => classSymbol.genericArguments.size
//          case _ => 0
//        }
//
//        if (expectedArgs != typeRef.args.size) {
//          diagnostics += IncorrectNumberOfGenericArguments(
//            expectedArgs,
//            symbol.name,
//            typeRef.args.size,
//            typeRef.range
//          )
//          (GlobalScope.ErrorType, diagnostics)
//        } else {
//          // TODO - Get rid of variance from typeRef
//          val genericArguments = typeRef.args.map((typeRef, _) => resolveType(typeRef, scope) ~> diagnostics)
//
//          if (genericArguments.contains(GlobalScope.ErrorType)) {
//            (GlobalScope.ErrorType, diagnostics)
//          } else {
//            (SemanticType(symbol, genericArguments), diagnostics)
//          }
//        }
//      }
//      case None => {
//        diagnostics += CannotResolveSymbol(
//          typeRef.name.prettyPrint,
//          typeRef.range
//        )
//        (GlobalScope.ErrorType, diagnostics)
//      }
//    }
//  }
//}
