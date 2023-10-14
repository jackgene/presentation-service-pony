use "collections"
use "regex"

primitive NormalizedWordsTokenizing
  fun stop_words(): Set[String] val =>
    recover
      let words: Set[String] =
        HashSet[String, HashEq[String]](where prealloc = 101)

      words.union(
        [
          "about"
          "above"
          "after"
          "again"
          "against"
          "all"
          "and"
          "any"
          "are"
          "because"
          "been"
          "before"
          "being"
          "below"
          "between"
          "both"
          "but"
          "can"
          "did"
          "does"
          "doing"
          "down"
          "during"
          "each"
          "few"
          "for"
          "from"
          "further"
          "had"
          "has"
          "have"
          "having"
          "her"
          "here"
          "hers"
          "herself"
          "him"
          "himself"
          "his"
          "how"
          "into"
          "its"
          "itself"
          "just"
          "me"
          "more"
          "most"
          "myself"
          "nor"
          "not"
          "now"
          "off"
          "once"
          "only"
          "other"
          "our"
          "ours"
          "ourselves"
          "out"
          "over"
          "own"
          "same"
          "she"
          "should"
          "some"
          "such"
          "than"
          "that"
          "the"
          "their"
          "theirs"
          "them"
          "themselves"
          "then"
          "there"
          "these"
          "they"
          "this"
          "those"
          "through"
          "too"
          "under"
          "until"
          "very"
          "was"
          "were"
          "what"
          "when"
          "where"
          "which"
          "while"
          "who"
          "whom"
          "why"
          "will"
          "with"
          "you"
          "your"
          "yours"
          "yourself"
          "yourselves"
        ].values()
      )

      words
    end

class NormalizedWordsTokenizer
  let _env: Env val
  let _stop_words: Set[String] val
  let _min_word_length: USize
  let _max_word_length: USize
  let _valid_word_pattern: _StringMatcher val
  let _word_separator_pattern: _StringSplitter val

  new val create(
    env: Env,
    stop_words: Set[String] val = recover HashSet[String, HashEq[String]] end,
    min_word_length: USize = 1,
    max_word_length: USize = USize.max_value()
  ) =>
    _env = env
    _stop_words =
      recover
        let lower_cased_stop_words = HashSet[String, HashEq[String]](
          where prealloc = stop_words.size()
        )
        for stop_word in stop_words.values() do
          lower_cased_stop_words.set(stop_word.lower())
        end

        lower_cased_stop_words
      end
    _min_word_length = min_word_length
    _max_word_length = max_word_length
    _valid_word_pattern =
      try
        recover Regex("""(\p{L}+(?:-\p{L}+)*)""")? end
      else
        _env.err.print("[PROGRAMMING ERROR] bad _valid_word_pattern regex")
        // Fake matcher that never matches
        object val is _StringMatcher
          fun box eq(subject: (String box | Array[U8 val] box)): Bool val =>
            _env.err.print("[PROGRAMMING ERROR] bad _valid_word_pattern regex")
            false
        end
      end
    _word_separator_pattern =
      try
        recover Regex("""[^\p{L}\-]+""")? end
      else
        _env.err.print("[PROGRAMMING ERROR] _word_separator_pattern regex")
        // Fake spliter that always fails
        object val is _StringSplitter
          fun box split(
            subject: String val, offset: USize val = 0
          ): Array[String val] iso^ ? =>
            _env.err.print("[PROGRAMMING ERROR] _word_separator_pattern regex")
            error
        end
      end

  fun val apply(text: String val): Array[String val] iso^ =>
    recover
      let text': String val =
        recover
          let text' = text.clone()
          text'.strip()
          consume text'
        end
      let words: Array[String] =
        try
          _word_separator_pattern.split(text')?
        else [] end

      let tokens: Array[String] = Array[String](where len = words.size())
      for word in words.values() do
        let word': String val =
          recover
            let word' = word.lower()
            word'.strip("-")
            consume word'
          end
        if
          (_valid_word_pattern == word') and
          (_min_word_length <= word'.size()) and
          (word'.size() <= _max_word_length) and
          (not _stop_words.contains(word'))
        then tokens.push(word') end
      end

      tokens
    end
