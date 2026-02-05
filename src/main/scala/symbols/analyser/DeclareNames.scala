package symbols.analyser

import ast.Modifier.{OVERRIDE, PRIVATE, PROTECTED, PUBLIC}
import ast.{ClassDecl, Decl, FunctionDecl, KahwaFile, Modifier, ModifierNode, TypeParameterDecl, TypedefDecl, VariableDecl}
import diagnostics.Diagnostic
import diagnostics.Diagnostic.{IllegalModifierCombination, ModifierNotAllowed, RepeatedModifier, SymbolAlreadyDeclared}
import sources.SourceRange
import symbols.{ClassSymbol, FieldSymbol, FunctionSymbol, MethodSymbol, Scope, Symbol, TranslationUnit, TypeParameterSymbol, TypedefSymbol, VariableSymbol, Visibility, VisibleVariableSymbol}
import symbols.analyser.SemanticAnalyser.MutableNodeToSymbol

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

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

    diagnostics ++= repeatedModifiers(modifiers)

    diagnostics ++= illegalCombinations(modifiers, Map(
      Modifier.PUBLIC -> Set(Modifier.PRIVATE, Modifier.PROTECTED),
      Modifier.PRIVATE -> Set(Modifier.PUBLIC, Modifier.PROTECTED),
      Modifier.PROTECTED -> Set(Modifier.PUBLIC, Modifier.PRIVATE)
    ))

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

    diagnostics ++= illegalCombinations(modifiers, Map(
      Modifier.OPEN -> Set(Modifier.FINAL),
      Modifier.FINAL -> Set(Modifier.ABSTRACT, Modifier.OPEN),
      Modifier.ABSTRACT -> Set(Modifier.FINAL)
    ))

    diagnostics ++= repeatedModifiers(modifiers)

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

  private def repeatedModifiers(modifiers: List[ModifierNode]): List[Diagnostic] = {
    val found = mutable.Set[Modifier]()
    val diagnostics = ListBuffer[Diagnostic]()
    modifiers.foreach { modifierNode =>
      if (found.contains(modifierNode.modifier)) diagnostics += RepeatedModifier(modifierNode.modifier, modifierNode.range)
      found += modifierNode.modifier
    }
    diagnostics.toList
  }

  private def illegalCombinations(modifiers: List[ModifierNode], modifierPairs: Map[Modifier, Set[Modifier]]): List[Diagnostic] = {
    val found = mutable.Set[Modifier]()
    val diagnostics = ListBuffer[Diagnostic]()
    modifiers.foreach { modifierNode =>
      val illegalCombinations = modifierPairs.getOrElse(modifierNode.modifier, Set.empty) intersect found
      illegalCombinations.foreach(diagnostics += IllegalModifierCombination(modifierNode.modifier, _, modifierNode.range))
      found += modifierNode.modifier
    }
    diagnostics.toList
  }
}
