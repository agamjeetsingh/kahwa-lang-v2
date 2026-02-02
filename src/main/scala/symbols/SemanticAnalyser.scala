package symbols

import ast.Modifier.{FINAL, OVERRIDE, PRIVATE, PROTECTED, PUBLIC, STATIC}
import ast.{AstNode, AstTransformer, BinaryExpr, CallExpr, ClassDecl, Decl, Expr, FunctionDecl, Ident, IndexExpr, KahwaFile, LiteralExpr, MemberAccessExpr, Modifier, ModifierNode, Qual, TernaryExpr, TraversingVisitor, TypeParameterDecl, TypeRef, TypedefDecl, UnaryExpr, Unqual, VariableDecl}
import diagnostics.Diagnostic
import diagnostics.Diagnostic.{CannotResolveSymbol, IllegalModifierCombination, IncorrectNumberOfGenericArguments, ModifierNotAllowed, RepeatedModifier, SymbolAlreadyDeclared, TypedefCycleDetected}
import sources.SourceRange
import symbols.AstSymbolExtensions.*

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

type NodeToSymbol = Map[Decl, Symbol]
type IdentToSymbol = Map[Ident, Symbol]
type NodeToScope = Map[AstNode, Scope]

object SemanticAnalyser {
  private type MutableNodeToSymbol = mutable.Map[Decl, Symbol]
  private type MutableIdentToSymbol = mutable.Map[Ident, Symbol]
  private type MutableNodeToScope = mutable.Map[AstNode, Scope]
  def processFile(file: KahwaFile): (TranslationUnit, List[Diagnostic]) = {
    var kahwaFile = file

    // Phase 1: Compress member access expressions to unqual idents ((a.b).c) -> (a.b.c)
    kahwaFile = AccessCompressor.transform(kahwaFile)

    given nodeToSymbol: MutableNodeToSymbol = mutable.Map()
    given diagnostics: ListBuffer[Diagnostic] = ListBuffer()

    // Phase 2: Declare all top-level functions, top-level variables, classes, fields, methods and function/method parameters
    val res = DeclareNames.declareFile(kahwaFile)

    // Phase 3: Provide a scope to every single AST Node (TODO)
    val nodeToScope: MutableNodeToScope = mutable.Map.empty

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

  extension [T](symbolAndDiagnostics: (T, Iterable[Diagnostic])) {
    private def ~>(sink: ListBuffer[Diagnostic]): T = {
      sink ++= symbolAndDiagnostics._2
      symbolAndDiagnostics._1
    }
  }

  private object DeclareNames {
    def declareFile(kahwaFile: KahwaFile)(using nodeToSymbol: MutableNodeToSymbol, diagnostics: ListBuffer[Diagnostic]): TranslationUnit = {
      val translationUnit = TranslationUnit(kahwaFile.range.fileId.toString, List.empty)

      registerType(
        translationUnit,
        kahwaFile.classDecls,
        classDecl => (declareClass(classDecl, translationUnit.scope, true), SourceRange.dummy),
        translationUnit.classes += _
      )

      registerTerm(
        translationUnit,
        kahwaFile.functionDecls,
        functionDecl => (declareFunction(functionDecl, translationUnit.scope), SourceRange.dummy),
        translationUnit.functions += _,
        duplicatesAllowed = true
      )

      registerType(
        translationUnit,
        kahwaFile.variableDecls,
        variableDecl => (declareVisibleVariable(variableDecl, translationUnit.scope), SourceRange.dummy),
        translationUnit.variables += _
      )

      registerType(
        translationUnit,
        kahwaFile.typedefDecls,
        typedefDecl => (declareTypedef(typedefDecl, translationUnit.scope), SourceRange.dummy),
        translationUnit.typedefs += _
      )

      translationUnit
    }

    private def register[T <: Decl, U <: Symbol](parentSymbol: Symbol,
                                                 decls: List[T],
                                                 declToSymbolAndRange: T => (U, SourceRange),
                                                 registerSymbol: U => Unit,
                                                 duplicatesAllowed: Boolean, term: Boolean)
                                                (using nodeToSymbol: MutableNodeToSymbol): List[Diagnostic] = {
      val ts = decls.map(decl => (declToSymbolAndRange(decl), decl))

      ts.flatMap(tuple => {
        val ((childSymbol, range), decl) = tuple
        val badDuplicate = !duplicatesAllowed && (
          (term && parentSymbol.scope.searchForTerm(childSymbol.name).nonEmpty)
            || (!term && parentSymbol.scope.searchForType(childSymbol.name).nonEmpty))
        nodeToSymbol += decl -> childSymbol
        if (badDuplicate) {
          List(SymbolAlreadyDeclared(childSymbol.name, range))
        } else {
          parentSymbol.scope.define(childSymbol)
          registerSymbol(childSymbol)
          List.empty
        }
      })
    }

    private def registerTerm[T <: Decl, U <: Symbol](parentSymbol: Symbol,
                                                     decls: List[T],
                                                     declToSymbolAndRange: T => (U, SourceRange),
                                                     registerSymbol: U => Unit,
                                                     duplicatesAllowed: Boolean = false)
                                                    (using nodeToSymbol: MutableNodeToSymbol, diagnostics: ListBuffer[Diagnostic]): Unit = {
      diagnostics ++= register(parentSymbol, decls, declToSymbolAndRange, registerSymbol, duplicatesAllowed, true)
    }

    private def registerType[T <: Decl, U <: Symbol](parentSymbol: Symbol,
                                                     decls: List[T],
                                                     declToSymbolAndRange: T => (U, SourceRange),
                                                     registerSymbol: U => Unit,
                                                     duplicatesAllowed: Boolean = false)
                                                    (using nodeToSymbol: MutableNodeToSymbol, diagnostics: ListBuffer[Diagnostic]): Unit = {
      diagnostics ++= register(parentSymbol, decls, declToSymbolAndRange, registerSymbol, duplicatesAllowed, false)
    }

    private def declareClass(classDecl: ClassDecl, outerScope: Scope, topLevel: Boolean)(using nodeToSymbol: MutableNodeToSymbol, diagnostics: ListBuffer[Diagnostic]): ClassSymbol = {
      val classSymbol = ClassSymbol(classDecl.name, outerScope)
      
      registerType(
        classSymbol,
        classDecl.typeParameters,
        (typeParameterDecl: TypeParameterDecl) => (TypeParameterSymbol(typeParameterDecl.name, classSymbol.scope, typeParameterDecl.variance), typeParameterDecl.range),
        classSymbol.genericArguments += _
      )

      registerType(
        classSymbol,
        classDecl.nestedClasses,
        nestedClassDecl => (declareClass(nestedClassDecl, classSymbol.scope, false), classDecl.range),
        classSymbol.nestedClasses += _
      )

      registerTerm(
        classSymbol,
        classDecl.methods,
        methodDecl => (declareMethod(methodDecl, classSymbol.scope), methodDecl.range),
        classSymbol.methods += _,
        duplicatesAllowed = true
      )

      registerTerm(
        classSymbol,
        classDecl.fields,
        variableDecl => (declareField(variableDecl, classSymbol.scope), variableDecl.range),
        classSymbol.fields += _
      )

      classSymbol.visibility = resolveVisibility(classDecl.modifiers, topLevel)

      modifierNotAllowed(classDecl.modifiers, Set(Modifier.STATIC, Modifier.OVERRIDE).contains)

      classSymbol.setModality(resolveModality(classDecl.modifiers))

      classSymbol
    }

    private def declareFunction(functionDecl: FunctionDecl, outerScope: Scope)(using nodeToSymbol: MutableNodeToSymbol, diagnostics: ListBuffer[Diagnostic]): FunctionSymbol = {
      val functionSymbol = FunctionSymbol(functionDecl.name, outerScope)

      registerType(
        functionSymbol,
        functionDecl.typeParameters,
        (typeParameterDecl: TypeParameterDecl) => (TypeParameterSymbol(typeParameterDecl.name, functionSymbol.scope, typeParameterDecl.variance), typeParameterDecl.range),
        functionSymbol.genericArguments += _
      )

      registerTerm(
        functionSymbol,
        functionDecl.parameters,
        variableDecl => (declareVariable(variableDecl, functionSymbol.scope), variableDecl.range),
        functionSymbol.parameters += _
      )

      functionSymbol.visibility = resolveVisibility(functionDecl.modifiers, true)
      functionSymbol.isStatic = hasModifier(functionDecl.modifiers, Modifier.STATIC)

      modifierNotAllowed(functionDecl.modifiers, modifier => modifier.isModality || modifier == OVERRIDE)

      functionSymbol
    }

    private def declareTypedef(typedefDecl: TypedefDecl, outerScope: Scope)(using nodeToSymbol: MutableNodeToSymbol, diagnostics: ListBuffer[Diagnostic]): TypedefSymbol = {
      val typedefSymbol = TypedefSymbol(typedefDecl.name, outerScope)

      registerType(
        typedefSymbol,
        typedefDecl.typeParameters,
        (typeParameterDecl: TypeParameterDecl) => (TypeParameterSymbol(typeParameterDecl.name, typedefSymbol.scope, typeParameterDecl.variance), typeParameterDecl.range),
        typedefSymbol.genericArguments += _
      )

      typedefSymbol.visibility = resolveVisibility(typedefDecl.modifiers, true)
      modifierNotAllowed(typedefDecl.modifiers, !_.isVisibility)

      typedefSymbol
    }

    private def declareMethod(functionDecl: FunctionDecl, outerScope: Scope)(using nodeToSymbol: MutableNodeToSymbol, diagnostics: ListBuffer[Diagnostic]): MethodSymbol = {
      val methodSymbol = MethodSymbol(functionDecl.name, outerScope)

      registerType(
        methodSymbol,
        functionDecl.typeParameters,
        (typeParameterDecl: TypeParameterDecl) => (TypeParameterSymbol(typeParameterDecl.name, methodSymbol.scope, typeParameterDecl.variance), typeParameterDecl.range),
        methodSymbol.genericArguments += _
      )

      registerTerm(
        methodSymbol,
        functionDecl.parameters,
        variableDecl => (declareVariable(variableDecl, methodSymbol.scope), variableDecl.range),
        methodSymbol.parameters += _
      )

      methodSymbol.visibility = resolveVisibility(functionDecl.modifiers, false)
      methodSymbol.isStatic = hasModifier(functionDecl.modifiers, Modifier.STATIC)

      methodSymbol.setModality(resolveModality(functionDecl.modifiers))
      methodSymbol.isAnOverride = hasModifier(functionDecl.modifiers, Modifier.OVERRIDE)

      methodSymbol
    }

    private def declareVariable(variableDecl: VariableDecl, outerScope: Scope)(using diagnostics: ListBuffer[Diagnostic]): VariableSymbol = {
      val variableSymbol = VariableSymbol(variableDecl.name, outerScope)

      variableSymbol.isStatic = hasModifier(variableDecl.modifiers, Modifier.STATIC)
      modifierNotAllowed(variableDecl.modifiers, _ != Modifier.STATIC)

      variableSymbol
    }

    private def declareVisibleVariable(variableDecl: VariableDecl, outerScope: Scope)(using diagnostics: ListBuffer[Diagnostic]): VisibleVariableSymbol = {
      val visibleVariableSymbol = VisibleVariableSymbol(variableDecl.name, outerScope)

      visibleVariableSymbol.isStatic = hasModifier(variableDecl.modifiers, Modifier.STATIC)
      visibleVariableSymbol.visibility = resolveVisibility(variableDecl.modifiers, true)

      modifierNotAllowed(variableDecl.modifiers, modifier => modifier.isModality || modifier == OVERRIDE)

      visibleVariableSymbol
    }

    private def declareField(variableDecl: VariableDecl, outerScope: Scope)(using diagnostics: ListBuffer[Diagnostic]): FieldSymbol = {
      val fieldSymbol = FieldSymbol(variableDecl.name, outerScope)

      fieldSymbol.isStatic = hasModifier(variableDecl.modifiers, Modifier.STATIC)
      fieldSymbol.visibility = resolveVisibility(variableDecl.modifiers, true)

      fieldSymbol.setModality(resolveModality(variableDecl.modifiers))
      fieldSymbol.isAnOverride = hasModifier(variableDecl.modifiers, Modifier.OVERRIDE)

      fieldSymbol
    }

    private def resolveVisibility(allModifiers: List[ModifierNode], topLevel: Boolean)(using diagnostics: ListBuffer[Diagnostic]): Visibility = {
      val modifiers = allModifiers.filter(_.modifier.isVisibility)
      
      val res = if (modifiers.isEmpty) {
        Visibility.PUBLIC
      } else if (modifiers.size == 1) {
        if (topLevel && modifiers.head.modifier == Modifier.PROTECTED) {
          Visibility.PUBLIC
        } else {
          Visibility.fromModifier(modifiers.head.modifier)
        }
      } else {
        Visibility.fromModifier(modifiers.head.modifier)
      }

      var publicFound = false
      var protectedFound = false
      var privateFound = false

      modifiers.foreach(modifierNode => modifierNode.modifier match {
        case Modifier.PUBLIC => {
          if ((!topLevel && protectedFound) || privateFound) {
            diagnostics += IllegalModifierCombination(if (protectedFound) PROTECTED else PRIVATE, PUBLIC, modifierNode.range)
          } else if (publicFound) {
            diagnostics += RepeatedModifier(PUBLIC, modifierNode.range)
          }
          publicFound = true
        }
        case Modifier.PRIVATE => {
          if ((!topLevel && protectedFound) || publicFound) {
            diagnostics += IllegalModifierCombination(if (protectedFound) PROTECTED else PUBLIC, PRIVATE, modifierNode.range)
          } else if (privateFound) {
            diagnostics += RepeatedModifier(PRIVATE, modifierNode.range)
          }
          privateFound = true
        }
        case Modifier.PROTECTED => {
          if (topLevel) {
            diagnostics += ModifierNotAllowed(PROTECTED, modifierNode.range)
          } else if (publicFound || privateFound) {
            diagnostics += IllegalModifierCombination(if (publicFound) PUBLIC else PRIVATE, PROTECTED, modifierNode.range)
          } else if (protectedFound) {
            diagnostics += RepeatedModifier(PROTECTED, modifierNode.range)
          }
          protectedFound = true
        }
        case _ => throw IllegalStateException("Should be unreachable")
      })

      res
    }

    private def modifierNotAllowed(modifiers: List[ModifierNode], notAllowed: Modifier => Boolean)(using diagnostics: ListBuffer[Diagnostic]): Unit = {
      diagnostics ++= modifiers.collect(modifierNode => modifierNode.modifier match {
        case modifier if notAllowed(modifier) => ModifierNotAllowed(modifier, modifierNode.range)
      })
    }

    private def resolveModality(allModifiers: List[ModifierNode])(using diagnostics: ListBuffer[Diagnostic]): Modifier = {
      val modifiers = allModifiers.filter(_.modifier.isModality)
      
      val res = if (modifiers.nonEmpty) {
        if (modifiers.exists(_.modifier == Modifier.ABSTRACT)) {
          Modifier.ABSTRACT
        } else if (modifiers.exists(_.modifier == Modifier.OPEN)) {
          Modifier.OPEN
        } else {
          Modifier.FINAL
        }
      } else {
        Modifier.FINAL
      }

      var finalFound = false
      var openFound = false
      var abstractFound = false

      modifiers.foreach(modifierNode => modifierNode.modifier match {
        case Modifier.OPEN => {
          if (finalFound) diagnostics += IllegalModifierCombination(Modifier.FINAL, Modifier.OPEN, modifierNode.range)
          else if (openFound) diagnostics += RepeatedModifier(Modifier.OPEN, modifierNode.range)
          openFound = true
        }
        case Modifier.FINAL => {
          if (abstractFound || openFound) diagnostics += IllegalModifierCombination(if (abstractFound) Modifier.ABSTRACT else Modifier.OPEN, Modifier.FINAL, modifierNode.range)
          else if (finalFound) diagnostics += RepeatedModifier(Modifier.FINAL, modifierNode.range)
          finalFound = true
        }
        case Modifier.ABSTRACT => {
          if (finalFound) diagnostics += IllegalModifierCombination(Modifier.FINAL, Modifier.ABSTRACT, modifierNode.range)
          else if (abstractFound) diagnostics += RepeatedModifier(Modifier.ABSTRACT, modifierNode.range)
          abstractFound = true
        }
        case _ => throw IllegalStateException("Should be unreachable")
      })

      res
    }

    private def hasModifier(modifiers: List[ModifierNode], desiredModifier: Modifier)(using diagnostics: ListBuffer[Diagnostic]): Boolean = {
      var found = false
      modifiers.foreach(modifierNode => {
        if (modifierNode.modifier == desiredModifier) {
          if (!found) found = true
          else {
            diagnostics += RepeatedModifier(desiredModifier, modifierNode.range)
          }
        }
      })
      found
    }
  }

  private class QualifyIdents(nodeToScope: NodeToScope) extends AstTransformer {
    given NodeToScope = nodeToScope
    override def transform(typeRef: TypeRef): TypeRef = {
      nodeToScope(typeRef).searchForType(typeRef.name.asInstanceOf[Unqual]).headOption match {
        case Some(typeSymbol) => TypeRef(
          Qual(typeRef.name.asInstanceOf[Unqual].name, typeSymbol, typeRef.name.range),
          typeRef.args.map((typeRef, variance) => (transform(typeRef), variance)),
          typeRef.range)
        case None => super.transform(typeRef)
      }
    }

    override def transform(expr: Expr): Expr = {
      expr match {
        case ident: Ident => ident.asInstanceOf[Unqual];
        case BinaryExpr(expr1, expr2, op, range) => ???
        case UnaryExpr(expr, op, range) => ???
        case CallExpr(callee, args, range) => ???
        case IndexExpr(callee, arg, range) => ???
        case MemberAccessExpr(base, member, range) => ???
        case _ => super.transform(expr)
      }
    }
  }

  private object PartialTypeResolver {
    class TypeResolver(val nodeToSymbol: NodeToSymbol, val nodeToScope: NodeToScope) extends TraversingVisitor[ListBuffer[Diagnostic]] {
      given NodeToSymbol = nodeToSymbol
      override protected def defaultResult: ListBuffer[Diagnostic] = ListBuffer()

      override protected def combine(r1: ListBuffer[Diagnostic], r2: ListBuffer[Diagnostic]): ListBuffer[Diagnostic] = r1 ++ r2

      override def visitKahwaFile(node: KahwaFile): ListBuffer[Diagnostic] = {
        withScope(node) {
          super.visitKahwaFile(node)
        }
      }

      override def visitTypedefDecl(node: TypedefDecl): ListBuffer[Diagnostic] = {
        val (semanticType, diagnostics) = resolveAndRecurse(node.referredType, super.visitTypedefDecl)(node)
        node.symbol.referredType = semanticType
        diagnostics
      }

      override def visitClassDecl(node: ClassDecl): ListBuffer[Diagnostic] = {
        resolveAndRecurse(node.symbol.superClasses, node.superClasses, super.visitClassDecl)(node)
      }

      override def visitFunctionDecl(node: FunctionDecl): ListBuffer[Diagnostic] = {
        val (semanticType, diagnostics) = resolveAndRecurse(node.returnType, super.visitFunctionDecl)(node)
        node.symbol.returnType = semanticType
        diagnostics
      }

      override def visitVariableDecl(node: VariableDecl): ListBuffer[Diagnostic] = {
        val (semanticType, diagnostics) = resolveAndRecurse(node.typeRef, super.visitVariableDecl)(node)
        node.symbol.semanticType = semanticType
        diagnostics
      }

      private def resolveAndRecurse[T <: AstNode](semanticTypes: ListBuffer[SemanticType], typeRefs: List[TypeRef], recursiveVisitor: T => ListBuffer[Diagnostic])(node: T) = {
        val diagnostics = ListBuffer[Diagnostic]()
        semanticTypes ++= typeRefs.map(resolveType(_, stack.top) ~> diagnostics)

        withScope(node) {
          diagnostics ++ recursiveVisitor(node)
        }
      }

      private def resolveAndRecurse[T <: AstNode](typeRef: TypeRef, recursiveVisitor: T => ListBuffer[Diagnostic])(node: T): (SemanticType, ListBuffer[Diagnostic]) = {
        val diagnostics = ListBuffer[Diagnostic]()
        val res = resolveType(typeRef, stack.top) ~> diagnostics

        withScope(node) {
          (res, diagnostics ++ recursiveVisitor(node))
        }
      }

      private def withScope[R](astNode: AstNode)(body: => R): R = {
        stack.push(nodeToScope(astNode))
        try {
          body
        } finally {
          stack.pop()
        }
      }

      private val stack: mutable.Stack[Scope] = mutable.Stack()
    }

    private def resolveType(typeRef: TypeRef, scope: Scope): (SemanticType, ListBuffer[Diagnostic]) = {
      val symbols = scope.searchForType(typeRef.name.prettyPrint)
      val diagnostics: ListBuffer[Diagnostic] = ListBuffer()
      symbols.headOption match {
        case Some(symbol) => {
          val expectedArgs = symbol match {
            case classSymbol: ClassSymbol => classSymbol.genericArguments.size
            case _ => 0
          }

          if (expectedArgs != typeRef.args.size) {
            diagnostics += IncorrectNumberOfGenericArguments(expectedArgs, symbol.name, typeRef.args.size, typeRef.range)
            (GlobalScope.ErrorType, diagnostics)
          } else {
            // TODO - Get rid of variance from typeRef
            val genericArguments = typeRef.args.map((typeRef, _) => resolveType(typeRef, scope) ~> diagnostics)

            if (genericArguments.contains(GlobalScope.ErrorType)) {
              (GlobalScope.ErrorType, diagnostics)
            } else {
              (SemanticType(symbol, genericArguments), diagnostics)
            }
          }
        }
        case None => {
          diagnostics += CannotResolveSymbol(typeRef.name.prettyPrint, typeRef.range)
          (GlobalScope.ErrorType, diagnostics)
        }
      }
    }
  }

  private object TypedefCycleDetector {
    def detectCycles(allTypedefs: List[TypedefDecl])(using nodeToSymbol: MutableNodeToSymbol): List[Diagnostic] = {
      val visited = mutable.Set[String]()
      val visiting = mutable.Set[String]()
      val diagnostics = ListBuffer[Diagnostic]()

      def dfs(typedefName: String, path: List[String]): Unit = {
        if (visiting.contains(typedefName)) {
          diagnostics += TypedefCycleDetected(path.reverse, SourceRange.dummy) // TODO - Source range
          return
        }
        if (visited.contains(typedefName)) return

        visiting += typedefName
        // Get all typedef dependencies
        for (dep <- getTypedefDependencies(typedefName, allTypedefs)) {
          dfs(dep, typedefName :: path)
        }
        visiting -= typedefName
        visited += typedefName
      }

      allTypedefs.foreach(td => dfs(td.name, List(td.name)))
      diagnostics.toList
    }

    private def getTypedefDependencies(name: String, allTypedefs: List[TypedefDecl])(using nodeToSymbol: MutableNodeToSymbol): List[String] = {
      allTypedefs.collect {
        case typedefDecl if nodeToSymbol.contains(typedefDecl) => typedefDecl
      }.find(_.name == name) match {
        case Some(typedefDecl) => {
          val typeRef = typedefDecl.referredType
          // TODO
          typeRef.name.prettyPrint :: typeRef.args.map(_._1.name.prettyPrint)
        }
        case None => List.empty
      }
    }
  }

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

  object AccessCompressor extends AstTransformer {
    override def transform(expr: Expr): Expr = {
      expr match {
        case MemberAccessExpr(base, member, range) => transform(base) match {
          case ident: Ident => Unqual(ident.head, ident.tail ++ List(member), expr.range)
          case _ => super.transform(expr)
        }
        case _ => super.transform(expr)
      }
    }
  }
}
