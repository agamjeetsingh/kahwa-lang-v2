package symbols

import ast.Modifier.{FINAL, OVERRIDE, PRIVATE, PROTECTED, PUBLIC, STATIC}
import ast.{ClassDecl, Decl, FunctionDecl, KahwaFile, Modifier, ModifierNode, TypeParameterDecl, TypeRef, VariableDecl}
import diagnostics.Diagnostic
import diagnostics.Diagnostic.{IllegalModifierCombination, ModifierNotAllowed, RepeatedModifier, SymbolAlreadyDeclared}
import sources.SourceRange

import scala.collection.mutable.ListBuffer

object SemanticAnalyser {
  def processFile(file: KahwaFile): (TranslationUnit, List[Diagnostic]) = {
    var kahwaFile = file
    val res = DeclareNames.declareFile(kahwaFile)

    kahwaFile = TypedefReplacer.replaceTypedefs(kahwaFile)

    res
  }

  private object DeclareNames {
    private def register[T <: Decl, U <: Symbol](parentSymbol: Symbol,
                                                 decls: List[T],
                                                 declToSymbolAndRange: T => (U, SourceRange),
                                                 registerSymbol: U => Unit,
                                                 duplicatesAllowed: Boolean, term: Boolean): List[Diagnostic] = {
      val ts = decls.map(decl => (declToSymbolAndRange(decl), decl))

      ts.flatMap(tuple => {
        val ((childSymbol, range), decl) = tuple
        if (!duplicatesAllowed && ((term && parentSymbol.scope.searchForTerm(childSymbol.name).nonEmpty) || (!term && parentSymbol.scope.searchForType(childSymbol.name).nonEmpty))) {
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
                                                     duplicatesAllowed: Boolean = false): List[Diagnostic] = {
      register(parentSymbol, decls, declToSymbolAndRange, registerSymbol, duplicatesAllowed, true)
    }

    private def registerType[T <: Decl, U <: Symbol](parentSymbol: Symbol,
                                                     decls: List[T],
                                                     declToSymbolAndRange: T => (U, SourceRange),
                                                     registerSymbol: U => Unit,
                                                     duplicatesAllowed: Boolean = false): List[Diagnostic] = {
      register(parentSymbol, decls, declToSymbolAndRange, registerSymbol, duplicatesAllowed, false)
    }

    extension [T](symbolAndDiagnostics: (T, Iterable[Diagnostic])) {
      def ~>(sink: ListBuffer[Diagnostic]): T = {
        sink ++= symbolAndDiagnostics._2
        symbolAndDiagnostics._1
      }
    }

    def declareFile(kahwaFile: KahwaFile): (TranslationUnit, List[Diagnostic]) = {
      val translationUnit = TranslationUnit(kahwaFile.range.fileId.toString, List.empty)
      val diagnostics = ListBuffer[Diagnostic]()

      registerType(
        translationUnit,
        kahwaFile.classDecls,
        classDecl => (declareClass(classDecl, translationUnit.scope, true) ~> diagnostics, SourceRange.dummy),
        translationUnit.classes += _
      )

      registerTerm(
        translationUnit,
        kahwaFile.functionDecls,
        functionDecl => (declareFunction(functionDecl, translationUnit.scope) ~> diagnostics, SourceRange.dummy),
        translationUnit.functions += _,
        duplicatesAllowed = true
      )

      registerType(
        translationUnit,
        kahwaFile.variableDecls,
        variableDecl => (declareVisibleVariable(variableDecl, translationUnit.scope) ~> diagnostics, SourceRange.dummy),
        translationUnit.variables += _
      )

      (translationUnit, diagnostics.toList)
    }

    private def declareClass(classDecl: ClassDecl, outerScope: Scope, topLevel: Boolean): (ClassSymbol, List[Diagnostic]) = {
      val classSymbol = ClassSymbol(classDecl.name, outerScope)
      val diagnostics = ListBuffer[Diagnostic]()

      registerType(
        classSymbol,
        classDecl.typeParameters,
        (typeParameterDecl: TypeParameterDecl) => (TypeParameterSymbol(typeParameterDecl.name, classSymbol.scope, typeParameterDecl.variance), typeParameterDecl.range),
        classSymbol.genericArguments += _
      )

      registerType(
        classSymbol,
        classDecl.nestedClasses,
        nestedClassDecl => (declareClass(nestedClassDecl, classSymbol.scope, false) ~> diagnostics, classDecl.range),
        classSymbol.nestedClasses += _
      )

      registerTerm(
        classSymbol,
        classDecl.methods,
        methodDecl => (declareMethod(methodDecl, classSymbol.scope) ~> diagnostics, methodDecl.range),
        classSymbol.methods += _,
        duplicatesAllowed = true
      )

      registerTerm(
        classSymbol,
        classDecl.fields,
        variableDecl => (declareField(variableDecl, classSymbol.scope) ~> diagnostics, variableDecl.range),
        classSymbol.fields += _
      )

      classSymbol.visibility = resolveVisibility(classDecl.modifiers, topLevel) ~> diagnostics

      diagnostics ++= modifierNotAllowed(classDecl.modifiers, Set(Modifier.STATIC, Modifier.OVERRIDE).contains)

      classSymbol.setModality(resolveModality(classDecl.modifiers) ~> diagnostics)

      (classSymbol, diagnostics.toList)
    }

    private def declareFunction(functionDecl: FunctionDecl, outerScope: Scope): (FunctionSymbol, List[Diagnostic]) = {
      val functionSymbol = FunctionSymbol(functionDecl.name, outerScope)
      val diagnostics = ListBuffer[Diagnostic]()

      registerType(
        functionSymbol,
        functionDecl.typeParameters,
        (typeParameterDecl: TypeParameterDecl) => (TypeParameterSymbol(typeParameterDecl.name, functionSymbol.scope, typeParameterDecl.variance), typeParameterDecl.range),
        functionSymbol.genericArguments += _
      )

      registerTerm(
        functionSymbol,
        functionDecl.parameters,
        variableDecl => (declareVariable(variableDecl, functionSymbol.scope) ~> diagnostics, variableDecl.range),
        functionSymbol.parameters += _
      )

      functionSymbol.visibility = resolveVisibility(functionDecl.modifiers, true) ~> diagnostics
      functionSymbol.isStatic = hasModifier(functionDecl.modifiers, Modifier.STATIC) ~> diagnostics

      diagnostics ++= modifierNotAllowed(functionDecl.modifiers, modifier => modifier.isModality || modifier == OVERRIDE)

      (functionSymbol, diagnostics.toList)
    }

    private def declareMethod(functionDecl: FunctionDecl, outerScope: Scope): (MethodSymbol, List[Diagnostic]) = {
      val methodSymbol = MethodSymbol(functionDecl.name, outerScope)
      val diagnostics = ListBuffer[Diagnostic]()

      registerType(
        methodSymbol,
        functionDecl.typeParameters,
        (typeParameterDecl: TypeParameterDecl) => (TypeParameterSymbol(typeParameterDecl.name, methodSymbol.scope, typeParameterDecl.variance), typeParameterDecl.range),
        methodSymbol.genericArguments += _
      )

      registerTerm(
        methodSymbol,
        functionDecl.parameters,
        variableDecl => (declareVariable(variableDecl, methodSymbol.scope) ~> diagnostics, variableDecl.range),
        methodSymbol.parameters += _
      )

      methodSymbol.visibility = resolveVisibility(functionDecl.modifiers, false) ~> diagnostics
      methodSymbol.isStatic = hasModifier(functionDecl.modifiers, Modifier.STATIC) ~> diagnostics

      methodSymbol.setModality(resolveModality(functionDecl.modifiers) ~> diagnostics)
      methodSymbol.isAnOverride = hasModifier(functionDecl.modifiers, Modifier.OVERRIDE) ~> diagnostics

      (methodSymbol, diagnostics.toList)
    }

    private def declareVariable(variableDecl: VariableDecl, outerScope: Scope): (VariableSymbol, List[Diagnostic]) = {
      val variableSymbol = VariableSymbol(variableDecl.name, outerScope)
      val diagnostics = ListBuffer[Diagnostic]()

      variableSymbol.isStatic = hasModifier(variableDecl.modifiers, Modifier.STATIC) ~> diagnostics
      diagnostics ++= modifierNotAllowed(variableDecl.modifiers, _ != Modifier.STATIC)

      (variableSymbol, diagnostics.toList)
    }

    private def declareVisibleVariable(variableDecl: VariableDecl, outerScope: Scope): (VisibleVariableSymbol, List[Diagnostic]) = {
      val visibleVariableSymbol = VisibleVariableSymbol(variableDecl.name, outerScope)
      val diagnostics = ListBuffer[Diagnostic]()

      visibleVariableSymbol.isStatic = hasModifier(variableDecl.modifiers, Modifier.STATIC) ~> diagnostics
      visibleVariableSymbol.visibility = resolveVisibility(variableDecl.modifiers, true) ~> diagnostics

      diagnostics ++= modifierNotAllowed(variableDecl.modifiers, modifier => modifier.isModality || modifier == OVERRIDE)

      (visibleVariableSymbol, diagnostics.toList)
    }

    private def declareField(variableDecl: VariableDecl, outerScope: Scope): (FieldSymbol, List[Diagnostic]) = {
      val fieldSymbol = FieldSymbol(variableDecl.name, outerScope)
      val diagnostics = ListBuffer[Diagnostic]()

      fieldSymbol.isStatic = hasModifier(variableDecl.modifiers, Modifier.STATIC) ~> diagnostics
      fieldSymbol.visibility = resolveVisibility(variableDecl.modifiers, true) ~> diagnostics

      fieldSymbol.setModality(resolveModality(variableDecl.modifiers) ~> diagnostics)
      fieldSymbol.isAnOverride = hasModifier(variableDecl.modifiers, Modifier.OVERRIDE) ~> diagnostics

      (fieldSymbol, diagnostics.toList)
    }

    private def resolveVisibility(allModifiers: List[ModifierNode], topLevel: Boolean): (Visibility, List[Diagnostic]) = {
      val modifiers = allModifiers.filter(_.modifier.isVisibility)

      val diagnostics = ListBuffer[Diagnostic]()

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
            diagnostics += IllegalModifierCombination(if (protectedFound) PROTECTED else PRIVATE, PRIVATE, modifierNode.range)
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

      (res, diagnostics.toList)
    }

    private def modifierNotAllowed(modifiers: List[ModifierNode], notAllowed: Modifier => Boolean): List[Diagnostic] = {
      modifiers.collect(modifierNode => modifierNode.modifier match {
        case modifier if notAllowed(modifier) => ModifierNotAllowed(modifier, modifierNode.range)
      })
    }

    private def resolveModality(allModifiers: List[ModifierNode]): (Modifier, List[Diagnostic]) = {
      val modifiers = allModifiers.filter(_.modifier.isModality)

      val diagnostics = ListBuffer[Diagnostic]()

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

      (res, diagnostics.toList)
    }

    private def hasModifier(modifiers: List[ModifierNode], desiredModifier: Modifier): (Boolean, List[Diagnostic]) = {
      val diagnostics = ListBuffer[Diagnostic]()
      var found = false
      modifiers.foreach(modifierNode => {
        if (modifierNode.modifier == desiredModifier) {
          if (!found) found = true
          else {
            diagnostics += RepeatedModifier(desiredModifier, modifierNode.range)
          }
        }
      })
      (found, diagnostics.toList)
    }
  }

  private class TypedefReplacer(typedefMap: Map[String, TypeRef]) extends AstTransformer {
    override def transform(typeRef: TypeRef): TypeRef = {
      // Look up the name in the typedef map
      typedefMap.get(typeRef.name.name) match {
        case Some(targetType) =>
          // Found a typedef! Replace it and recurse (for typedef chains)
          transform(targetType.copy(range = typeRef.range))
        case None =>
          // Not a typedef, but still recurse into generic arguments
          super.transform(typeRef)
      }
    }
  }

  private object TypedefReplacer {
    def replaceTypedefs(kahwaFile: KahwaFile): KahwaFile = {
      val typedefMap = kahwaFile.typedefDecls.map { typedef =>
        typedef.name -> typedef.referredType
      }.toMap

      val replacer = TypedefReplacer(typedefMap)
      replacer.transform(kahwaFile)
    }
  }
}
