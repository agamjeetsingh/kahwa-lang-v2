import parser.{Parser, SafePointFunction, Token, Tokeniser}
import sources.SourceRange
import symbols.analyser.SemanticAnalyser

import scala.io.Source
import scala.util.{Try, Using}

@main
def main(args: String*): Unit = {
  val (source, fileName) =
    args.headOption match {
      case Some(path) =>
        Using(Source.fromFile(path))(_.mkString).fold(
          err => { System.err.println(s"Error: cannot read '$path': ${err.getMessage}"); sys.exit(1) },
          content => (content, path)
        )
      case None =>
        val example = "def foo(): Int { val b = foo(); val x = 1; val y = true; val z = false; val a: Bool = {2; val t = z; val z = x; {z;}}; val w = y;}"
        System.err.println("Usage: sbt \"run <file.kw>\"")
        System.err.println("No file given — running on built-in example.\n")
        (example, "<example>")
    }

  val sourceLines = source.split('\n')

  // Convert a character offset to 1-indexed (line, col)
  def locationOf(pos: Int): (Int, Int) = {
    val before = source.take(pos)
    val line = before.count(_ == '\n') + 1
    val col = before.length - before.lastIndexOf('\n')
    (line, col)
  }

  def showDiagnostic(d: diagnostics.Diagnostic): Unit = {
    if d.range.fileId < 0 then
      println(s"  error: ${d.msg}")
    else {
      val (line, col) = locationOf(d.range.pos)
      println(s"  $fileName:$line:$col  error: ${d.msg}")
      sourceLines.lift(line - 1).foreach { srcLine =>
        val underlineLen = d.range.length.max(1).min(srcLine.length - col + 2)
        println(s"    $srcLine")
        println(s"    ${" " * (col - 1)}${"^" * underlineLen}")
      }
    }
    println()
  }

  def section(title: String): Unit = {
    val bar = "=" * (80 - title.length - 5)
    println(s"=== $title $bar")
  }

  // ── Tokenise ─────────────────────────────────────────────────────────────────
  val (tokens, tokenDiags) = Tokeniser.tokenise(source, 0)

  // ── Parse ─────────────────────────────────────────────────────────────────────
  given SafePointFunction[Token] = _ => false
  val (parseResult, _, parseDiags) = Parser.parseKahwaFile(tokens)

  // ── Parse Tree ────────────────────────────────────────────────────────────────
  section("Parse Tree")
  println()
  parseResult match {
    case None      => println("  (parse failed entirely)")
    case Some(ast) => println(ast.prettyPrint)
  }

  // ── Diagnostics ───────────────────────────────────────────────────────────────
  println()
  section("Diagnostics")
  println()

  val semanticDiags: List[diagnostics.Diagnostic] = parseResult match {
    case None => List.empty
    case Some(ast) =>
      Try(SemanticAnalyser.processFile(ast)).fold(
        err => {
          println(s"  [Note] Semantic analysis incomplete (some checks not yet implemented).")
          println(s"         ${err.getMessage}\n")
          List.empty
        },
        _._2
      )
  }

  val allDiags = (tokenDiags ++ parseDiags ++ semanticDiags).toList
  if allDiags.isEmpty then
    println("  No issues found.")
  else {
    println(s"  ${allDiags.length} issue(s):\n")
    allDiags.foreach(showDiagnostic)
  }
}