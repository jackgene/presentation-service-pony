interface _StringMatcher
  fun box eq(subject: (String box | Array[U8 val] box)): Bool val

interface _StringSplitter
  fun box split(
    subject: String val, offset: USize val = 0
  ): Array[String val] iso^ ?
