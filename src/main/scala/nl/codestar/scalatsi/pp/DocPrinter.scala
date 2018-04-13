package nl.codestar.scalatsi.pp

object DocPrinter{

  def pp(doc: Doc): String = {
    val ctx = new DocContext(0, 0, 0, StringBuilder.newBuilder)
    pp(doc, ctx)
    ctx.builder.toString()
  }

  private def pp(doc: Doc, ctx: DocContext): Unit = {
    doc match {
      case V(lhs, rhs) => {
        val h = ctx.h
        pp(lhs, ctx)
        crnl(ctx, h)
        pp(rhs, ctx)
      }
      case H(lhs, rhs) => {
        pp(lhs, ctx)
        pp(rhs, ctx)
      }
      case Text(s) => {
        var remainder = s
        while(remainder.nonEmpty){
          val charsHorizontal = Math.min(ctx.hLimit - ctx.h, remainder.length)
          val (toAppend, rest) = remainder.splitAt(charsHorizontal)
          ctx.builder.append(toAppend)
          remainder = rest
          if(remainder.nonEmpty){
            crnl(ctx)
          } else {
            ctx.h += charsHorizontal
          }
        }
      }
      case Sep(docs) => {
        if(docs.nonEmpty){
          pp(docs.reduceLeft(_ $$ _), ctx)
        }
      }
      case Nest(n, doc) => {
        ctx.hOffset += n
        if(ctx.h < ctx.hOffset) {
          linefeed(ctx, ctx.hOffset - ctx.h)
        }
        pp(doc, ctx)
        ctx.hOffset -= n
      }
      case Newline() => {
        crnl(ctx)
      }
      case Empty() =>
    }
  }

  private def crnl(ctx: DocContext): Unit = crnl(ctx, ctx.hOffset)
  private def crnl(ctx: DocContext, h: Int): Unit ={
    ctx.v += 1
    ctx.h = 0
    ctx.builder.append('\n')
    linefeed(ctx, h)
    ()
  }
  private def linefeed(ctx: DocContext, h: Int): Unit = {
    ctx.h += h
    ctx.builder.append(" " * h)
    ()
  }
}