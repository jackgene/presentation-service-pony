use "collections"

primitive Token
  fun languages_by_name(): Map[String, String] val =>
    let mapping: Map[String, String] iso =
      recover iso
        HashMap[String, String, HashEq[String]](
          where prealloc = 34
        )
      end

    // Others
    // C/C++
    mapping("c") = "C"
    mapping("c++") = "C"
    // C#
    mapping("c#") = "C#"
    mapping("csharp") = "C#"
    // Go
    mapping("go") = "Go"
    mapping("golang") = "Go"
    // Kotlin
    mapping("kotlin") = "Kotlin"
    mapping("kt") = "Kotlin"
    // Java
    mapping("java") = "Java"
    // Javascript
    mapping("js") = "JavaScript"
    mapping("ecmascript") = "JavaScript"
    mapping("javascript") = "JavaScript"
    // Lisp
    mapping("lisp") = "Lisp"
    mapping("clojure") = "Lisp"
    mapping("racket") = "Lisp"
    mapping("scheme") = "Lisp"
    // ML
    mapping("ml") = "ML"
    mapping("haskell") = "ML"
    mapping("caml") = "ML"
    mapping("elm") = "ML"
    mapping("f#") = "ML"
    mapping("ocaml") = "ML"
    mapping("purescript") = "ML"
    // Perl
    mapping("perl") = "Perl"
    // PHP
    mapping("php") = "PHP"
    // Python
    mapping("py") = "Python"
    mapping("python") = "Python"
    // Ruby
    mapping("ruby") = "Ruby"
    mapping("rb") = "Ruby"
    // Rust
    mapping("rust") = "Rust"
    // Scala
    mapping("scala") = "Scala"
    // Swift
    mapping("swift") = "Swift"
    // TypeScript
    mapping("ts") = "TypeScript"
    mapping("typescript") = "TypeScript"

    mapping

class TokenFromFirstWord
  let _token_by_word: Map[String, String] val

  new val create(token_by_word: Map[String, String] val) =>
    _token_by_word = token_by_word

  fun val apply(text: String val): (String val| None) =>
    let text' = text.clone()
    text'.strip()
    let words: Array[String val] = text'.split(where n = 2)

    try
      _token_by_word(words.shift()?.lower())?
    else
      None
    end
