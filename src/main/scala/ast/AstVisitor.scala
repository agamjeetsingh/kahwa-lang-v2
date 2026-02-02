package ast

/**
 * Base visitor trait for traversing AST nodes.
 * Type parameter R represents the result type returned by visit methods.
 */
trait Visitor[R] {
  // ===== Literal Expressions =====
  def visitBoolLiteral(node: BoolLiteral): R
  def visitFloatLiteral(node: FloatLiteral): R
  def visitIntegerLiteral(node: IntegerLiteral): R
  def visitNullLiteral(node: NullLiteral): R
  def visitStringLiteral(node: StringLiteral): R

  // ===== Identifiers =====
  def visitUnqual(node: Unqual): R
  def visitQual(node: Qual): R

  // ===== Complex Expressions =====
  def visitBinaryExpr(node: BinaryExpr): R
  def visitUnaryExpr(node: UnaryExpr): R
  def visitCallExpr(node: CallExpr): R
  def visitIndexExpr(node: IndexExpr): R
  def visitMemberAccessExpr(node: MemberAccessExpr): R
  def visitTernaryExpr(node: TernaryExpr): R

  // ===== Statements =====
  def visitBreakStmt(node: BreakStmt): R
  def visitContinueStmt(node: ContinueStmt): R
  def visitExprStmt(node: ExprStmt): R
  def visitBlockStmt(node: BlockStmt): R
  def visitIfStmt(node: IfStmt): R
  def visitReturnStmt(node: ReturnStmt): R
  def visitWhileStmt(node: WhileStmt): R
  def visitVariableDeclStmt(node: VariableDeclStmt): R

  // ===== Declarations =====
  def visitTypedefDecl(node: TypedefDecl): R
  def visitVariableDecl(node: VariableDecl): R
  def visitTypeParameterDecl(node: TypeParameterDecl): R
  def visitFunctionDecl(node: FunctionDecl): R
  def visitClassDecl(node: ClassDecl): R

  // ===== Other Nodes =====
  def visitTypeRef(node: TypeRef): R
  def visitModifierNode(node: ModifierNode): R
  def visitKahwaFile(node: KahwaFile): R
}

/**
 * Extension methods to add accept functionality to AST nodes without modifying them.
 */
extension (node: AstNode) {
  def accept[R](visitor: Visitor[R]): R = node match {
    // Literals
    case n: BoolLiteral => visitor.visitBoolLiteral(n)
    case n: FloatLiteral => visitor.visitFloatLiteral(n)
    case n: IntegerLiteral => visitor.visitIntegerLiteral(n)
    case n: NullLiteral => visitor.visitNullLiteral(n)
    case n: StringLiteral => visitor.visitStringLiteral(n)

    // Identifiers (order matters: check Qual before Unqual since Qual extends Ident)
    case n: Qual => visitor.visitQual(n)
    case n: Unqual => visitor.visitUnqual(n)

    // Complex Expressions
    case n: BinaryExpr => visitor.visitBinaryExpr(n)
    case n: UnaryExpr => visitor.visitUnaryExpr(n)
    case n: CallExpr => visitor.visitCallExpr(n)
    case n: IndexExpr => visitor.visitIndexExpr(n)
    case n: MemberAccessExpr => visitor.visitMemberAccessExpr(n)
    case n: TernaryExpr => visitor.visitTernaryExpr(n)

    // Statements
    case n: BreakStmt => visitor.visitBreakStmt(n)
    case n: ContinueStmt => visitor.visitContinueStmt(n)
    case n: ExprStmt => visitor.visitExprStmt(n)
    case n: BlockStmt => visitor.visitBlockStmt(n)
    case n: IfStmt => visitor.visitIfStmt(n)
    case n: ReturnStmt => visitor.visitReturnStmt(n)
    case n: WhileStmt => visitor.visitWhileStmt(n)
    case n: VariableDeclStmt => visitor.visitVariableDeclStmt(n)

    // Declarations
    case n: TypedefDecl => visitor.visitTypedefDecl(n)
    case n: VariableDecl => visitor.visitVariableDecl(n)
    case n: TypeParameterDecl => visitor.visitTypeParameterDecl(n)
    case n: FunctionDecl => visitor.visitFunctionDecl(n)
    case n: ClassDecl => visitor.visitClassDecl(n)

    // Other nodes
    case n: TypeRef => visitor.visitTypeRef(n)
    case n: ModifierNode => visitor.visitModifierNode(n)
    case n: KahwaFile => visitor.visitKahwaFile(n)
  }
}

/**
 * A base visitor implementation that automatically traverses all child nodes.
 * Override specific visit methods to customize behavior for particular node types.
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

  // ===== Literal Expressions (leaves - no children to visit) =====
  def visitBoolLiteral(node: BoolLiteral): R = defaultResult
  def visitFloatLiteral(node: FloatLiteral): R = defaultResult
  def visitIntegerLiteral(node: IntegerLiteral): R = defaultResult
  def visitNullLiteral(node: NullLiteral): R = defaultResult
  def visitStringLiteral(node: StringLiteral): R = defaultResult

  // ===== Identifiers (leaves - no children to visit) =====
  def visitUnqual(node: Unqual): R = defaultResult
  def visitQual(node: Qual): R = defaultResult

  // ===== Complex Expressions =====
  def visitBinaryExpr(node: BinaryExpr): R = {
    val r1 = node.expr1.accept(this)
    val r2 = node.expr2.accept(this)
    combine(r1, r2)
  }

  def visitUnaryExpr(node: UnaryExpr): R = {
    node.expr.accept(this)
  }

  def visitCallExpr(node: CallExpr): R = {
    val r1 = node.callee.accept(this)
    val r2 = visitList(node.args)
    combine(r1, r2)
  }

  def visitIndexExpr(node: IndexExpr): R = {
    val r1 = node.callee.accept(this)
    val r2 = node.arg.accept(this)
    combine(r1, r2)
  }

  def visitMemberAccessExpr(node: MemberAccessExpr): R = node.base.accept(this)

  def visitTernaryExpr(node: TernaryExpr): R = {
    val r1 = node.cond.accept(this)
    val r2 = node.expr1.accept(this)
    val r3 = node.expr2.accept(this)
    combine(combine(r1, r2), r3)
  }

  // ===== Statements =====
  def visitBreakStmt(node: BreakStmt): R = defaultResult
  def visitContinueStmt(node: ContinueStmt): R = defaultResult

  def visitExprStmt(node: ExprStmt): R = {
    node.expr.accept(this)
  }

  def visitBlockStmt(node: BlockStmt): R = {
    visitList(node.stmts)
  }

  def visitIfStmt(node: IfStmt): R = {
    val r1 = node.expr.accept(this)
    val r2 = node.ifBlock.accept(this)
    val r3 = visitOption(node.elseBlock)
    combine(combine(r1, r2), r3)
  }

  def visitReturnStmt(node: ReturnStmt): R = {
    node.expr.accept(this)
  }

  def visitWhileStmt(node: WhileStmt): R = {
    val r1 = node.cond.accept(this)
    val r2 = node.body.accept(this)
    combine(r1, r2)
  }

  def visitVariableDeclStmt(node: VariableDeclStmt): R = {
    node.variableDecl.accept(this)
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