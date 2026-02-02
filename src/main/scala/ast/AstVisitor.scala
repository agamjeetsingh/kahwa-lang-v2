package ast

/**
 * Base visitor trait for traversing AST nodes.
 * Type parameter R represents the result type returned by visit methods.
 *
 * This uses a hybrid approach: generalized visitExpr/visitStmt for expressions and statements
 * (which are uniform in behavior), but specific methods for declarations (which have distinct semantics).
 */
trait Visitor[R] {
  // Generalized for expressions and statements
  def visitExpr(node: Expr): R
  def visitStmt(node: Stmt): R

  // Specific for declarations (they have distinct semantic meanings)
  def visitTypedefDecl(node: TypedefDecl): R
  def visitVariableDecl(node: VariableDecl): R
  def visitTypeParameterDecl(node: TypeParameterDecl): R
  def visitFunctionDecl(node: FunctionDecl): R
  def visitClassDecl(node: ClassDecl): R

  // Other nodes
  def visitTypeRef(node: TypeRef): R
  def visitModifierNode(node: ModifierNode): R
  def visitKahwaFile(node: KahwaFile): R
}

/**
 * Extension methods to add accept functionality to AST nodes without modifying them.
 */
extension (node: AstNode) {
  def accept[R](visitor: Visitor[R]): R = node match {
    // KahwaFile (must come first since it extends Decl)
    case n: KahwaFile => visitor.visitKahwaFile(n)

    // Generalized for expressions and statements
    case n: Expr => visitor.visitExpr(n)
    case n: Stmt => visitor.visitStmt(n)

    // Specific for declarations
    case n: TypedefDecl => visitor.visitTypedefDecl(n)
    case n: VariableDecl => visitor.visitVariableDecl(n)
    case n: TypeParameterDecl => visitor.visitTypeParameterDecl(n)
    case n: FunctionDecl => visitor.visitFunctionDecl(n)
    case n: ClassDecl => visitor.visitClassDecl(n)

    // Other nodes
    case n: TypeRef => visitor.visitTypeRef(n)
    case n: ModifierNode => visitor.visitModifierNode(n)
  }
}

/**
 * A base visitor implementation that automatically traverses all child nodes using pattern matching.
 * Override the general visit methods (visitExpr, visitStmt, visitDecl) to customize behavior.
 *
 * Subclasses must implement:
 * - defaultResult: The default value to return when visiting leaf nodes
 * - combine (optional): How to aggregate results from multiple children
 */
abstract class TraversingVisitor[R] extends Visitor[R] {
  /**
   * The default result to return for leaf nodes or when no custom logic is needed.
   */
  protected def defaultResult: R

  /**
   * Combines results from visiting multiple child nodes.
   * Default implementation returns the second result (useful for Unit, or keeping last result).
   * Override to implement custom aggregation logic (e.g., combining lists, accumulating values).
   */
  protected def combine(r1: R, r2: R): R = r2

  /**
   * Helper to visit a list of nodes and combine their results.
   */
  protected def visitList(nodes: List[AstNode]): R = {
    nodes.foldLeft(defaultResult)((acc, node) => combine(acc, node.accept(this)))
  }

  /**
   * Helper to visit an optional node.
   */
  protected def visitOption(nodeOpt: Option[AstNode]): R = {
    nodeOpt.map(_.accept(this)).getOrElse(defaultResult)
  }

  // ===== Expressions =====
  def visitExpr(node: Expr): R = node match {
    case n: BinaryExpr =>
      val r1 = n.expr1.accept(this)
      val r2 = n.expr2.accept(this)
      combine(r1, r2)

    case n: UnaryExpr =>
      n.expr.accept(this)

    case n: CallExpr =>
      val r1 = n.callee.accept(this)
      val r2 = visitList(n.args)
      combine(r1, r2)

    case n: IndexExpr =>
      val r1 = n.callee.accept(this)
      val r2 = n.arg.accept(this)
      combine(r1, r2)

    case n: MemberAccessExpr =>
      n.base.accept(this)

    case n: TernaryExpr =>
      val r1 = n.cond.accept(this)
      val r2 = n.expr1.accept(this)
      val r3 = n.expr2.accept(this)
      combine(combine(r1, r2), r3)

    // Leaf nodes (literals and identifiers)
    case _: Ident | _: BoolLiteral | _: FloatLiteral |
         _: IntegerLiteral | _: NullLiteral | _: StringLiteral =>
      defaultResult
  }

  // ===== Statements =====
  def visitStmt(node: Stmt): R = node match {
    case n: BlockStmt =>
      visitList(n.stmts)

    case n: ExprStmt =>
      n.expr.accept(this)

    case n: IfStmt =>
      val r1 = n.expr.accept(this)
      val r2 = n.ifBlock.accept(this)
      val r3 = visitOption(n.elseBlock)
      combine(combine(r1, r2), r3)

    case n: ReturnStmt =>
      n.expr.accept(this)

    case n: WhileStmt =>
      val r1 = n.cond.accept(this)
      val r2 = n.body.accept(this)
      combine(r1, r2)

    case n: VariableDeclStmt =>
      n.variableDecl.accept(this)

    // Leaf nodes
    case _: BreakStmt | _: ContinueStmt =>
      defaultResult
  }

  // ===== Declarations =====
  def visitTypedefDecl(node: TypedefDecl): R = {
    val r1 = visitList(node.typeParameters)
    val r2 = node.referredType.accept(this)
    val r3 = visitList(node.modifiers)
    combine(combine(r1, r2), r3)
  }

  def visitVariableDecl(node: VariableDecl): R = {
    val r1 = node.typeRef.accept(this)
    val r2 = visitOption(node.initExpr)
    val r3 = visitList(node.modifiers)
    combine(combine(r1, r2), r3)
  }

  def visitTypeParameterDecl(node: TypeParameterDecl): R = {
    visitList(node.modifiers)
  }

  def visitFunctionDecl(node: FunctionDecl): R = {
    val r1 = node.returnType.accept(this)
    val r2 = visitList(node.parameters)
    val r3 = node.block.accept(this)
    val r4 = visitList(node.modifiers)
    val r5 = visitList(node.typeParameters)
    combine(combine(combine(combine(r1, r2), r3), r4), r5)
  }

  def visitClassDecl(node: ClassDecl): R = {
    val r1 = visitList(node.modifiers)
    val r2 = visitList(node.superClasses)
    val r3 = visitList(node.fields)
    val r4 = visitList(node.methods)
    val r5 = visitList(node.nestedClasses)
    val r6 = visitList(node.typeParameters)
    combine(combine(combine(combine(combine(r1, r2), r3), r4), r5), r6)
  }

  // ===== Other Nodes =====
  def visitTypeRef(node: TypeRef): R = {
    val r1 = node.name.accept(this)
    val r2 = visitList(node.args.map(_._1)) // Visit type arguments (ignoring variance)
    combine(r1, r2)
  }

  def visitModifierNode(node: ModifierNode): R = defaultResult

  def visitKahwaFile(node: KahwaFile): R = {
    val r1 = visitList(node.typedefDecls)
    val r2 = visitList(node.classDecls)
    val r3 = visitList(node.functionDecls)
    val r4 = visitList(node.variableDecls)
    combine(combine(combine(r1, r2), r3), r4)
  }
}