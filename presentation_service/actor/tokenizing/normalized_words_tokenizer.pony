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
  let _valid_word_pattern: (Regex val | None) =
      try recover Regex("""(\p{L}+(?:-\p{L}+)*)""")? end else None end
  let _word_separator_pattern: (Regex val | None) =
      try recover Regex("""[^\p{L}\-]+""")? end else None end

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
    match _valid_word_pattern
    | None =>
      _env.err.print("[PROGRAMMING ERROR] _valid_word_pattern regex")
    end
    match _word_separator_pattern
    | None =>
      _env.err.print("[PROGRAMMING ERROR] _word_separator_pattern regex")
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
          match _word_separator_pattern
          | let word_separator_pattern: Regex val =>
            word_separator_pattern.split(text')?
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
        let word': String val =
          recover
            let word' = word.lower()
            word'.strip("-")
            consume word'
          end
        match _valid_word_pattern
        | let valid_word_pattern: Regex val =>
          if
            (valid_word_pattern == word') and
            (_min_word_length <= word'.size()) and
            (word'.size() <= _max_word_length) and
            (not _stop_words.contains(word'))
          then
            tokens.push(word')
          end
        end
      end

      tokens
    end
