package sources

case class SourceRange(fileId: Int, pos: Int, length: Int = 1) {
  infix def <->(other: SourceRange): SourceRange = {
    assert(fileId == other.fileId)

    val start = math.min(pos, other.pos)
    val end = math.max(pos + length, other.pos + other.length)
    SourceRange(fileId, start, end - start)
  }
}

object SourceRange {
  val dummy = SourceRange(-1, 0)
}
