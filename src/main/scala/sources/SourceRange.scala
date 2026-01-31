package sources

case class SourceRange(fileId: Int, pos: Int, length: Int = 1) {
  infix def <->(other: SourceRange): SourceRange = {
    assert(fileId == other.fileId)
    
    SourceRange(fileId, math.min(pos, other.pos), math.max(pos + length, other.pos + other.length))
  }
}

object SourceRange {
  val dummy = SourceRange(-1, 0)
}
