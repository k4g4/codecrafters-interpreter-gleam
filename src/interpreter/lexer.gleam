import interpreter/common

import gleam/bool
import gleam/float
import gleam/function
import gleam/int
import gleam/list
import gleam/option
import gleam/pair
import gleam/result
import gleam/string

type LexError {
  InvalidNumberError
  IdentError
  TagError(String)
  UntilError(String)
  OrError(LexError, LexError)
  AnyError
  Labelled(LexError, String)
  ShortCircuitAny(LexError)
}

fn lex_error_to_string(error: LexError) -> String {
  case error {
    InvalidNumberError -> "invalidnumber"
    IdentError -> "ident"
    TagError(tag) -> "tag(" <> tag <> ")"
    UntilError(until) -> "until(" <> until <> ")"
    OrError(first, second) ->
      "("
      <> lex_error_to_string(first)
      <> ", "
      <> lex_error_to_string(second)
      <> ")"
    AnyError -> "any"
    Labelled(error, label) -> lex_error_to_string(error) <> " (" <> label <> ")"
    ShortCircuitAny(error) -> lex_error_to_string(error)
  }
}

type LexResult(a) =
  Result(#(String, a), LexError)

type Lexer(a) =
  fn(String) -> LexResult(a)

const keywords = [
  common.KeywordAnd, common.KeywordClass, common.KeywordElse,
  common.KeywordFalse, common.KeywordFor, common.KeywordFun, common.KeywordIf,
  common.KeywordNil, common.KeywordOr, common.KeywordPrint, common.KeywordReturn,
  common.KeywordSuper, common.KeywordThis, common.KeywordTrue, common.KeywordVar,
  common.KeywordWhile,
]

fn keyword_to_pattern(keyword: common.KeywordToken) -> String {
  keyword
  |> string.inspect
  |> string.drop_left(string.length("Keyword"))
  |> string.lowercase
}

fn match_keyword(keyword: common.KeywordToken) -> Lexer(common.Token) {
  fn(in) {
    let pattern = keyword_to_pattern(keyword)
    use #(in, _) <- result.map(tag(pattern)(in))
    #(in, common.Keyword(keyword, pattern))
  }
}

const basic_tokens = [
  common.Paren(common.Left), common.Paren(common.Right),
  common.Brace(common.Left), common.Brace(common.Right), common.EqualEqual,
  common.BangEqual, common.LessEqual, common.GreaterEqual, common.Space,
  common.Tab, common.Newline, common.Equal, common.Bang, common.Less,
  common.Greater, common.Star, common.Dot, common.Comma, common.Plus,
  common.Minus, common.Slash, common.Semicolon,
]

fn match_basic_token(basic_token: common.BasicToken) -> Lexer(common.Token) {
  fn(in) {
    let pattern = common.basic_token_to_pattern(basic_token)
    use #(in, _) <- result.map(tag(pattern)(in))
    #(in, common.Basic(basic_token))
  }
}

fn comment(in: String) -> LexResult(common.Token) {
  use #(in, _) <- result.map(tag("//")(in))
  case string.split_once(in, "\n") {
    Ok(#(_, in)) -> #("\n" <> in, common.Comment)
    _ -> #("", common.Comment)
  }
}

fn string_literal(in: String) -> LexResult(common.Token) {
  use #(in, _) <- result.try(tag("\"")(in))
  let until_result = in |> until("\"") |> result.map_error(ShortCircuitAny)
  use #(in, contents) <- result.map(until_result)
  #(in, common.Literal(common.LiteralString, contents))
}

fn number_literal(in: String) -> LexResult(common.Token) {
  let is_digit = fn(c) { result.is_ok(int.parse(c)) }
  let assert Ok(#(in, digits)) = while(is_digit)(in)
  use <- bool.guard(digits == "", Error(InvalidNumberError))
  let assert Ok(#(in, maybe_decimal)) = maybe(tag("."))(in)
  case maybe_decimal {
    option.Some(_) -> {
      let assert Ok(#(in, decimal_digits)) = while(is_digit)(in)
      use <- bool.guard(decimal_digits == "", Error(InvalidNumberError))
      let all_digits = digits <> "." <> decimal_digits
      let assert Ok(number) = float.parse(all_digits)
      Ok(#(in, common.Literal(common.LiteralNumber(number), all_digits)))
    }
    option.None -> {
      let assert Ok(number) = digits |> int.parse |> result.map(int.to_float)
      Ok(#(in, common.Literal(common.LiteralNumber(number), digits)))
    }
  }
}

fn ident(in: String) -> LexResult(common.Token) {
  let #(a, z, underscore) = #(97, 122, 95)
  let #(a_up, z_up) = #(a - 32, z - 32)
  let is_alpha = fn(c) {
    { c >= a && c <= z } || { c >= a_up && c <= z_up } || c == underscore
  }
  let #(zero, nine) = #(48, 57)
  let is_alphanumeric = fn(c) { is_alpha(c) || { c >= zero && c <= nine } }
  let to_codepoint = fn(c) {
    c
    |> string.to_utf_codepoints
    |> list.first
    |> result.lazy_unwrap(fn() { panic })
    |> string.utf_codepoint_to_int
  }
  let pop_result =
    in |> string.pop_grapheme |> result.map_error(fn(_) { IdentError })
  use #(first, in) <- result.try(pop_result)
  case is_alpha(to_codepoint(first)) {
    True -> {
      let assert Ok(#(in, remainder)) =
        while(fn(c) { is_alphanumeric(to_codepoint(c)) })(in)
      Ok(#(in, common.Ident(first <> remainder)))
    }
    _ -> {
      Error(IdentError)
    }
  }
}

fn matcher() -> Lexer(common.Token) {
  basic_tokens
  |> list.map(match_basic_token)
  |> list.prepend(comment)
  |> list.prepend(string_literal)
  |> list.prepend(number_literal)
  |> list.prepend(ident)
  |> { function.flip(list.append) }(list.map(keywords, match_keyword))
  |> any
}

pub fn scan(in: String) -> common.Return {
  tokenized_to_return(tokenize(in, matcher()))
}

pub fn lex(in: String) -> Result(List(common.Token), String) {
  tokenize(in, matcher())
  |> list.try_map(fn(tokenized) {
    case tokenized {
      Token(token) -> Ok(Ok(token))
      TokenizedError(error: error, ..) ->
        Error(tokenized_error_to_string(error))
      Eof -> Ok(Error(Nil))
    }
  })
  |> result.map(list.filter_map(_, function.identity))
  |> result.map(list.reverse)
}

type TokenizedError {
  UnexpectedChar(char: String)
  Unterminated
}

fn tokenized_error_to_string(error: TokenizedError) -> String {
  case error {
    UnexpectedChar(char) -> "Unexpected character: " <> char
    Unterminated -> "Unterminated string."
  }
}

type Tokenized {
  Token(common.Token)
  TokenizedError(line: Int, error: TokenizedError)
  Eof
}

fn tokenized_is_error(tokenized) -> Bool {
  case tokenized {
    TokenizedError(..) -> True
    _ -> False
  }
}

fn tokenized_to_string(tokenized: Tokenized) -> String {
  case tokenized {
    Token(token) -> common.token_to_string(token)
    TokenizedError(line, error) ->
      "[line "
      <> int.to_string(line)
      <> "] Error: "
      <> tokenized_error_to_string(error)
    Eof -> "EOF  null"
  }
}

fn tokenized_to_return(tokenized: List(Tokenized)) -> common.Return {
  do_tokenized_to_return(tokenized, common.Return("", ""))
}

fn do_tokenized_to_return(
  tokenized: List(Tokenized),
  return: common.Return,
) -> common.Return {
  case tokenized {
    [] -> return
    [first, ..rest] -> {
      let stringified = tokenized_to_string(first)
      let return = case tokenized_is_error(first) {
        True ->
          common.Return(..return, error: stringified <> "\n" <> return.error)
        _ -> common.Return(..return, out: stringified <> "\n" <> return.out)
      }
      do_tokenized_to_return(rest, return)
    }
  }
}

fn tokenize(in: String, matcher: Lexer(common.Token)) -> List(Tokenized) {
  do_tokenize(in, matcher, 1, [])
}

fn do_tokenize(
  in: String,
  matcher: Lexer(common.Token),
  line: Int,
  tokenized: List(Tokenized),
) -> List(Tokenized) {
  case in {
    "" -> [Eof, ..tokenized]

    _ -> {
      case matcher(in) {
        Ok(#(in, common.Comment))
        | Ok(#(in, common.Basic(common.Space)))
        | Ok(#(in, common.Basic(common.Tab))) -> {
          do_tokenize(in, matcher, line, tokenized)
        }

        Ok(#(in, common.Basic(common.Newline))) -> {
          do_tokenize(in, matcher, line + 1, tokenized)
        }

        Ok(#(in, token)) -> {
          do_tokenize(in, matcher, line, [Token(token), ..tokenized])
        }

        Error(UntilError(_)) | Error(Labelled(UntilError(_), _)) -> {
          let in =
            in
            |> string.split_once("\n")
            |> result.map(pair.second)
            |> result.unwrap("")
          do_tokenize(in, matcher, line, [
            TokenizedError(line, Unterminated),
            ..tokenized
          ])
        }

        Error(AnyError) | Error(Labelled(..)) -> {
          let assert Ok(first) = string.first(in)
          let in = string.drop_left(in, 1)
          do_tokenize(in, matcher, line, [
            TokenizedError(line, UnexpectedChar(first)),
            ..tokenized
          ])
        }

        Error(error) ->
          panic as { "unexpected error: " <> lex_error_to_string(error) }
      }
    }
  }
}

fn tag(tag: String) -> Lexer(Nil) {
  fn(in) {
    case string.starts_with(in, tag) {
      True -> Ok(#(string.drop_left(in, string.length(tag)), Nil))
      _ -> Error(TagError(tag))
    }
  }
}

// fn tag_no_case(tag: String) -> Lexer(String) {
//   fn(in) {
//     let substr = string.slice(in, 0, string.length(tag))
//     case string.lowercase(substr) == string.lowercase(tag) {
//       True -> Ok(#(string.drop_left(in, string.length(tag)), substr))
//       _ -> Error(TagError(tag))
//     }
//   }
// }

fn until(until: String) -> Lexer(String) {
  fn(in) {
    in
    |> string.split_once(until)
    |> result.map(pair.swap)
    |> result.map_error(fn(_) { UntilError(until) })
  }
}

fn while(f: fn(String) -> Bool) -> Lexer(String) {
  fn(in) { Ok(while_inner(in, "", f)) }
}

fn while_inner(
  in: String,
  acc: String,
  f: fn(String) -> Bool,
) -> #(String, String) {
  case string.pop_grapheme(in) {
    Ok(#(first, in)) ->
      case f(first) {
        True -> while_inner(in, acc <> first, f)
        _ -> #(first <> in, acc)
      }
    _ -> #(in, acc)
  }
}

fn maybe(lexer: Lexer(a)) -> Lexer(option.Option(a)) {
  fn(in) {
    in
    |> lexer
    |> result.map(pair.map_second(_, option.Some))
    |> result.try_recover(fn(_) { Ok(#(in, option.None)) })
  }
}

// fn or(first: Lexer(a), second: Lexer(a)) -> Lexer(a) {
//   fn(in) {
//     use first_error <- result.try_recover(first(in))
//     use second_error <- result.try_recover(second(in))
//     Error(OrError(first_error, second_error))
//   }
// }

fn any(lexers: List(Lexer(a))) -> Lexer(a) {
  any_inner(_, lexers)
}

fn any_inner(in: String, lexers: List(Lexer(a))) -> LexResult(a) {
  case lexers {
    [] -> Error(AnyError)
    [lexer, ..lexers] -> {
      use error <- result.try_recover(lexer(in))
      case error {
        ShortCircuitAny(error) -> Error(error)
        _ -> any_inner(in, lexers)
      }
    }
  }
}
// fn label_error(lexer: Lexer(a), to_label: fn(String) -> String) -> Lexer(a) {
//   fn(in) { lexer(in) |> result.map_error(Labelled(_, to_label(in))) }
// }
