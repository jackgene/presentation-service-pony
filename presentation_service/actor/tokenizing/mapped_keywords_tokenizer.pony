use "collections"
use "regex"

primitive MappedKeywordsTokenizing
  fun languages_by_name(): Map[String, String] val =>
    recover
      let mapping: Map[String, String] =
        HashMap[String, String, HashEq[String]](where prealloc = 34)

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
    end

class MappedKeywordsTokenizer
  let _env: Env val
  let _word_separator_pattern: (Regex val | None) =
    try 
      recover Regex("""[\s!"&,./?|]""")? end
    else None end
  let _token_by_word: Map[String, String] val

  new val create(env: Env, token_by_word: Map[String, String] val) =>
    _env = env
    _token_by_word = token_by_word

  fun val apply(text: String val): Array[String val] iso^ =>
    recover
      let text' = text.clone()
      text'.strip()
      let words: Array[String] =
        try
          match _word_separator_pattern
          | let word_separator_pattern: Regex val =>
            word_separator_pattern.split(text'.clone())?
          else error end
        else
          _env.err.print(
            "[PROGRAMMING ERROR] error spliting words with regex, " +
            "falling back to simple split"
          )
          text'.split()
        end

      let tokens: Array[String] = Array[String](where len = words.size())
      for word in words.values() do
        try
          tokens.push(_token_by_word(word.lower())?)
        end
      end

      tokens
    end
