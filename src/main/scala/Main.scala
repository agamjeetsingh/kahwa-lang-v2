import parser.{Parser, SafePointFunction, Token, Tokeniser}
import symbols.analyser.SemanticAnalyser

@main
def main(): Unit = {
  val (input, ds) = Tokeniser.tokenise("def foo(): Int { val x = 1; val y = true; val z = {2; val z = x; {z;}}; val w = y;}", 0)
  println(s"Tokenisation diagnostics: ${ds.map(_.msg)}")
  given SafePointFunction[Token] = tok => false
  val prog = Parser.parseKahwaFile(input)
  println(s"Parsing diagnostics: ${prog._3.map(_.msg)}")
  println(prog._1.get.prettyPrint)
  val (tu, diagnostics) = SemanticAnalyser.processFile(prog._1.get)
  println(s"Semantic diagnostics: ${diagnostics.map(_.msg)}")
}