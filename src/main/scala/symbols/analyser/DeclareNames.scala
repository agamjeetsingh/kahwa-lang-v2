package symbols.analyser

import ast.Modifier.{OVERRIDE, PRIVATE, PROTECTED, PUBLIC}
import ast.{ClassDecl, Decl, FieldDecl, FunctionDecl, KahwaFile, Modifier, ModifierNode, ObjectDecl, TypeParameterDecl, TypedefDecl, VariableDecl}
import diagnostics.Diagnostic
import diagnostics.Diagnostic.{IllegalModifierCombination, ModifierNotAllowed, RepeatedModifier, SymbolAlreadyDeclared}
import sources.SourceRange
import symbols.{ClassSymbol, FieldSymbol, FunctionSymbol, MethodSymbol, ObjectSymbol, Scope, Symbol, TranslationUnit, TypeParameterSymbol, TypedefSymbol, VariableSymbol, Visibility, VisibleVariableSymbol}
import symbols.analyser.SemanticAnalyser.{MutableNodeToSymbol, SemanticContext}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

private class DeclareNames(
    val semanticContext: SemanticContext
) {
  def declareFile(kahwaFile: KahwaFile): TranslationUnit = {
    val translationUnit =
      TranslationUnit(kahwaFile.range.fileId.toString, List.empty)

    registerType(
      translationUnit,
      kahwaFile.classDecls,
      classDecl => declareClass(classDecl, translationUnit.scope, true),
      translationUnit.classes += _
    )

    registerTerm(
      translationUnit,
      kahwaFile.objectDecls,
      objectDecl => declareObject(objectDecl, translationUnit.scope, true),
      translationUnit.objects += _
    )

    registerTerm(
      translationUnit,
      kahwaFile.functionDecls,
      functionDecl => declareFunction(functionDecl, translationUnit.scope),
      translationUnit.functions += _,
      duplicatesAllowed = true
    )

    registerType(
      translationUnit,
      kahwaFile.variableDecls,
      fieldDecl => declareVisibleVariable(fieldDecl, translationUnit.scope),
      translationUnit.variables += _
    )

    registerType(
      translationUnit,
      kahwaFile.typedefDecls,
      typedefDecl => declareTypedef(typedefDecl, translationUnit.scope),
      translationUnit.typedefs += _
    )

    semanticContext.nodeToSymbol += kahwaFile -> translationUnit

    translationUnit
  }

  private def register[T <: Decl, U <: Symbol](
      parentSymbol: Symbol,
      decls: List[T],
      declToSymbol: T => U,
      registerSymbol: U => Unit,
      duplicatesAllowed: Boolean,
      term: Boolean
  ): List[Diagnostic] = {
    val ts = decls.map(decl => (declToSymbol(decl), decl.range, decl))

    ts.flatMap(tuple => {
      val (childSymbol, range, decl) = tuple
      val badDuplicate = !duplicatesAllowed && ((term && parentSymbol.scope
        .searchForTerm(childSymbol.name)
        .nonEmpty)
        || (!term && parentSymbol.scope
          .searchForType(childSymbol.name)
          .nonEmpty))
      semanticContext.nodeToSymbol += decl -> childSymbol
      if (badDuplicate) {
        List(SymbolAlreadyDeclared(childSymbol.name, range))
      } else {
        parentSymbol.scope.define(childSymbol)
        registerSymbol(childSymbol)
        List.empty
      }
    })
  }

  private def registerTerm[T <: Decl, U <: Symbol](
      parentSymbol: Symbol,
      decls: List[T],
      declToSymbolAndRange: T => U,
      registerSymbol: U => Unit,
      duplicatesAllowed: Boolean = false
  ): Unit = {
    semanticContext.diagnostics ++= register(
      parentSymbol,
      decls,
      declToSymbolAndRange,
      registerSymbol,
      duplicatesAllowed,
      true
    )
  }

  private def registerType[T <: Decl, U <: Symbol](
      parentSymbol: Symbol,
      decls: List[T],
      declToSymbolAndRange: T => U,
      registerSymbol: U => Unit,
      duplicatesAllowed: Boolean = false
  ): Unit = {
    semanticContext.diagnostics ++= register(
      parentSymbol,
      decls,
      declToSymbolAndRange,
      registerSymbol,
      duplicatesAllowed,
      false
    )
  }

  private def declareClass(
      classDecl: ClassDecl,
      outerScope: Scope,
      topLevel: Boolean
  ): ClassSymbol = {
    val classSymbol = ClassSymbol(classDecl.name, outerScope)

    registerType(
      classSymbol,
      classDecl.typeParameters,
      (typeParameterDecl: TypeParameterDecl) =>
        TypeParameterSymbol(typeParameterDecl.name, classSymbol.scope, typeParameterDecl.variance),
      classSymbol.genericArguments += _
    )

    registerType(
      classSymbol,
      classDecl.nestedClasses,
      nestedClassDecl => declareClass(nestedClassDecl, classSymbol.scope, false),
      classSymbol.nestedClasses += _
    )

    registerTerm(
      classSymbol,
      classDecl.nestedObjects,
      nestedObjectDecl => declareObject(nestedObjectDecl, classSymbol.scope, false),
      classSymbol.nestedObjects += _
    )

    registerTerm(
      classSymbol,
      classDecl.methods,
      methodDecl => declareMethod(methodDecl, classSymbol.scope),
      classSymbol.methods += _,
      duplicatesAllowed = true
    )

    registerTerm(
      classSymbol,
      classDecl.fields,
      fieldDecl => declareField(fieldDecl, classSymbol.scope),
      classSymbol.fields += _
    )

    classSymbol.visibility = resolveVisibility(classDecl.modifiers, topLevel)

    modifierNotAllowed(
      classDecl.modifiers,
      _ == Modifier.OVERRIDE
    )

    classSymbol.setModality(resolveModality(classDecl.modifiers))

    classSymbol
  }

  private def declareObject(objectDecl: ObjectDecl, outerScope: Scope, topLevel: Boolean): ObjectSymbol = {
    val objectSymbol = ObjectSymbol(objectDecl.name, outerScope)

    registerType(
      objectSymbol,
      objectDecl.nestedClasses,
      nestedClassDecl => declareClass(nestedClassDecl, objectSymbol.scope, false),
      objectSymbol.nestedClasses += _
    )

    registerTerm(
      objectSymbol,
      objectDecl.nestedObjects,
      nestedObjectDecl => declareObject(nestedObjectDecl, objectSymbol.scope, false),
      objectSymbol.nestedObjects += _
    )

    registerTerm(
      objectSymbol,
      objectDecl.methods,
      methodDecl => declareMethod(methodDecl, objectSymbol.scope),
      objectSymbol.methods += _,
      duplicatesAllowed = true
    )

    registerTerm(
      objectSymbol,
      objectDecl.fields,
      fieldDecl => declareField(fieldDecl, objectSymbol.scope),
      objectSymbol.fields += _
    )

    objectSymbol.visibility = resolveVisibility(objectDecl.modifiers, topLevel)

    modifierNotAllowed(objectDecl.modifiers, _ == Modifier.OVERRIDE)

    objectSymbol.setModality(resolveModality(objectDecl.modifiers))

    objectSymbol
  }

  private def declareFunction(functionDecl: FunctionDecl, outerScope: Scope): FunctionSymbol = {
    val functionSymbol = FunctionSymbol(functionDecl.name, outerScope)

    registerType(
      functionSymbol,
      functionDecl.typeParameters,
      (typeParameterDecl: TypeParameterDecl) =>
        TypeParameterSymbol(typeParameterDecl.name, functionSymbol.scope, typeParameterDecl.variance),
      functionSymbol.genericArguments += _
    )

    registerTerm(
      functionSymbol,
      functionDecl.parameters,
      variableDecl => declareVariable(variableDecl, functionSymbol.scope),
      functionSymbol.parameters += _
    )

    functionSymbol.visibility = resolveVisibility(functionDecl.modifiers, true)

    modifierNotAllowed(
      functionDecl.modifiers,
      modifier => modifier.isModality || modifier == OVERRIDE
    )
    
    functionSymbol
  }

  private def declareTypedef(typedefDecl: TypedefDecl, outerScope: Scope): TypedefSymbol = {
    val typedefSymbol = TypedefSymbol(typedefDecl.name, outerScope)

    registerType(
      typedefSymbol,
      typedefDecl.typeParameters,
      (typeParameterDecl: TypeParameterDecl) =>
        TypeParameterSymbol(typeParameterDecl.name, typedefSymbol.scope, typeParameterDecl.variance),
      typedefSymbol.genericArguments += _
    )

    typedefSymbol.visibility = resolveVisibility(typedefDecl.modifiers, true)
    modifierNotAllowed(typedefDecl.modifiers, !_.isVisibility)

    typedefSymbol
  }

  private def declareMethod(functionDecl: FunctionDecl, outerScope: Scope): MethodSymbol = {
    val methodSymbol = MethodSymbol(functionDecl.name, outerScope)

    registerType(
      methodSymbol,
      functionDecl.typeParameters,
      (typeParameterDecl: TypeParameterDecl) =>
        TypeParameterSymbol(typeParameterDecl.name, methodSymbol.scope, typeParameterDecl.variance),
      methodSymbol.genericArguments += _
    )

    registerTerm(
      methodSymbol,
      functionDecl.parameters,
      variableDecl => declareVariable(variableDecl, methodSymbol.scope),
      methodSymbol.parameters += _
    )

    methodSymbol.visibility = resolveVisibility(functionDecl.modifiers, false)

    methodSymbol.setModality(resolveModality(functionDecl.modifiers))
    methodSymbol.isAnOverride =
      hasModifier(functionDecl.modifiers, Modifier.OVERRIDE)
    
    methodSymbol
  }

  private def declareVariable(variableDecl: VariableDecl, outerScope: Scope): VariableSymbol = {
    val variableSymbol = VariableSymbol(variableDecl.name, outerScope)

    variableSymbol
  }

  private def declareVisibleVariable(
      fieldDecl: FieldDecl,
      outerScope: Scope
  ): VisibleVariableSymbol = {
    val visibleVariableSymbol =
      VisibleVariableSymbol(fieldDecl.name, outerScope)

    visibleVariableSymbol.visibility =
      resolveVisibility(fieldDecl.modifiers, true)

    modifierNotAllowed(
      fieldDecl.modifiers,
      modifier => modifier.isModality || modifier == OVERRIDE
    )

    visibleVariableSymbol
  }

  private def declareField(fieldDecl: FieldDecl, outerScope: Scope): FieldSymbol = {
    val fieldSymbol = FieldSymbol(fieldDecl.name, outerScope)

    fieldSymbol.visibility = resolveVisibility(fieldDecl.modifiers, true)

    fieldSymbol.setModality(resolveModality(fieldDecl.modifiers))
    fieldSymbol.isAnOverride =
      hasModifier(fieldDecl.modifiers, Modifier.OVERRIDE)

    fieldSymbol
  }

  private def resolveVisibility(
      allModifiers: List[ModifierNode],
      topLevel: Boolean
  ): Visibility = {
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

    semanticContext.diagnostics ++= repeatedModifiers(modifiers)

    semanticContext.diagnostics ++= illegalCombinations(
      modifiers,
      Map(
        Modifier.PUBLIC -> Set(Modifier.PRIVATE, Modifier.PROTECTED),
        Modifier.PRIVATE -> Set(Modifier.PUBLIC, Modifier.PROTECTED),
        Modifier.PROTECTED -> Set(Modifier.PUBLIC, Modifier.PRIVATE)
      )
    )

    res
  }

  private def modifierNotAllowed(
      modifiers: List[ModifierNode],
      notAllowed: Modifier => Boolean
  ): Unit = {
    semanticContext.diagnostics ++= modifiers.collect(modifierNode =>
      modifierNode.modifier match {
        case modifier if notAllowed(modifier) =>
          ModifierNotAllowed(modifier, modifierNode.range)
      }
    )
  }

  private def resolveModality(
      allModifiers: List[ModifierNode]
  ): Modifier = {
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

    semanticContext.diagnostics ++= illegalCombinations(
      modifiers,
      Map(
        Modifier.OPEN -> Set(Modifier.FINAL),
        Modifier.FINAL -> Set(Modifier.ABSTRACT, Modifier.OPEN),
        Modifier.ABSTRACT -> Set(Modifier.FINAL)
      )
    )

    semanticContext.diagnostics ++= repeatedModifiers(modifiers)

    res
  }

  private def hasModifier(
      modifiers: List[ModifierNode],
      desiredModifier: Modifier
  ): Boolean = {
    var found = false
    modifiers.foreach(modifierNode => {
      if (modifierNode.modifier == desiredModifier) {
        if (!found) found = true
        else {
          semanticContext.diagnostics += RepeatedModifier(desiredModifier, modifierNode.range)
        }
      }
    })
    found
  }

  private def repeatedModifiers(
      modifiers: List[ModifierNode]
  ): List[Diagnostic] = {
    val found = mutable.Set[Modifier]()
    val diagnostics = ListBuffer[Diagnostic]()
    modifiers.foreach { modifierNode =>
      if (found.contains(modifierNode.modifier))
        diagnostics += RepeatedModifier(
          modifierNode.modifier,
          modifierNode.range
        )
      found += modifierNode.modifier
    }
    diagnostics.toList
  }

  private def illegalCombinations(
      modifiers: List[ModifierNode],
      modifierPairs: Map[Modifier, Set[Modifier]]
  ): List[Diagnostic] = {
    val found = mutable.Set[Modifier]()
    val diagnostics = ListBuffer[Diagnostic]()
    modifiers.foreach { modifierNode =>
      val illegalCombinations = modifierPairs.getOrElse(
        modifierNode.modifier,
        Set.empty
      ) intersect found
      illegalCombinations.foreach(
        diagnostics += IllegalModifierCombination(
          modifierNode.modifier,
          _,
          modifierNode.range
        )
      )
      found += modifierNode.modifier
    }
    diagnostics.toList
  }
}
