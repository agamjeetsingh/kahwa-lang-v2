package symbols

import ast.Modifier.{FINAL, PRIVATE, PROTECTED, PUBLIC}
import ast.{ClassDecl, Decl, FunctionDecl, KahwaFile, Modifier, ModifierNode, TypeParameterDecl, VariableDecl}
import diagnostics.Diagnostic
import diagnostics.Diagnostic.{IllegalModifierCombination, ModifierNotAllowed, RepeatedModifier, SymbolAlreadyDeclared}
import parser.Token.Public
import sources.SourceRange

import scala.collection.mutable.ListBuffer

object SemanticAnalyser {
  def processFile(kahwaFile: KahwaFile): TranslationUnit = {
    val res = declareFile(kahwaFile)

    ???
  }

  private def register[T <: Decl, U <: Symbol](parentSymbol: Symbol,
                                               decls: List[T],
                                               declToSymbolAndRange: T => (U, SourceRange),
                                               registerSymbol: U => Unit,
                                               duplicatesAllowed: Boolean, term: Boolean): List[Diagnostic] = {
    val ts = decls.map(decl => (declToSymbolAndRange(decl), decl))

    ts.flatMap(tuple => {
      val ((childSymbol, range), decl) = tuple
      if (!duplicatesAllowed && ((term && parentSymbol.scope.searchForTerm(childSymbol.name).nonEmpty) || parentSymbol.scope.searchForType(childSymbol.name).nonEmpty)) {
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

  extension[T] (symbolAndDiagnostics: (T, Iterable[Diagnostic])) {
    def ~>(sink: ListBuffer[Diagnostic]): T = {
      sink ++= symbolAndDiagnostics._2
      symbolAndDiagnostics._1
    }
  }

  private def declareFile(kahwaFile: KahwaFile): (TranslationUnit, List[Diagnostic]) = {
    val translationUnit = TranslationUnit(kahwaFile.range.fileId.toString, List.empty)
    val diagnostics = ListBuffer[Diagnostic]()

    registerType(
      translationUnit,
      kahwaFile.classDecls,
      classDecl => (declareClass(classDecl, translationUnit.scope, true) ~> diagnostics, SourceRange.dummy),
      translationUnit.classes += _)

    (translationUnit, diagnostics.toList)
  }

  private def declareClass(classDecl: ClassDecl, outerScope: Scope, topLevel: Boolean): (ClassSymbol, List[Diagnostic]) = {
    val classSymbol = ClassSymbol(classDecl.name, outerScope)
    val diagnostics = ListBuffer[Diagnostic]()

    registerType(
      classSymbol,
      classDecl.typeParameters,
      (typeParameterDecl : TypeParameterDecl) => (TypeParameterSymbol(typeParameterDecl.name, classSymbol.scope, typeParameterDecl.variance), typeParameterDecl.range),
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
    
    classSymbol.visibility = resolveVisibility(classDecl.modifiers, topLevel) ~> diagnostics

    diagnostics ++= modifierNotAllowed(classDecl.modifiers, Set(Modifier.STATIC, Modifier.OVERRIDE).contains)

//    classSymbol.setModality(resolveModality(classDecl.modifiers) ~> diagnostics)

    (classSymbol, diagnostics.toList)
  }

  private def declareMethod(functionDecl: FunctionDecl, outerScope: Scope): MethodSymbol = {
    val methodSymbol = MethodSymbol(functionDecl.name, outerScope)

    methodSymbol
  }

  private def declareField(variableDecl: VariableDecl, outerScope: Scope): FieldSymbol = {
    ???
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
      if (modifiers.contains(Modifier.ABSTRACT)) {
        Modifier.ABSTRACT
      } else if (modifiers.contains(Modifier.OPEN)) {
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
}
