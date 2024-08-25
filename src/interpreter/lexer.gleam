import gleam/int
import gleam/list
import gleam/result
import gleam/string

type LexError {
  TagError(String)
  OrError(LexError, LexError)
  AnyError
  Labelled(LexError, String)
}

fn error_to_string(error: LexError) -> String {
  case error {
    TagError(tag) -> "tag(" <> tag <> ")"
    OrError(first, second) ->
      "(" <> error_to_string(first) <> ", " <> error_to_string(second) <> ")"
    AnyError -> "any"
    Labelled(error, label) -> error_to_string(error) <> " (" <> label <> ")"
  }
}

type LexResult(a) =
  Result(#(String, a), LexError)

type Lexer(a) =
  fn(String) -> LexResult(a)

type Dir {
  Left
  Right
}

fn dir_to_string(dir: Dir) -> String {
  case dir {
    Left -> "LEFT"
    Right -> "RIGHT"
  }
}

type Token {
  Paren(Dir)
  Brace(Dir)
  EqualEqual
  BangEqual
  Equal
  Bang
  Star
  Dot
  Comma
  Plus
  Minus
  Semicolon
}

const all_tokens = [
  Paren(Left), Paren(Right), Brace(Left), Brace(Right), EqualEqual, BangEqual, Equal, Bang, Star,
  Dot, Comma, Plus, Minus, Semicolon,
]

fn token_to_pattern(token: Token) -> String {
  case token {
    Paren(Left) -> "("
    Paren(Right) -> ")"
    Brace(Left) -> "{"
    Brace(Right) -> "}"
    EqualEqual -> "=="
    BangEqual -> "!="
    Equal -> "="
    Bang -> "!"
    Star -> "*"
    Dot -> "."
    Comma -> ","
    Plus -> "+"
    Minus -> "-"
    Semicolon -> ";"
  }
}

fn token_to_string(token: Token) -> String {
  case token {
    Paren(dir) -> dir_to_string(dir) <> "_PAREN"
    Brace(dir) -> dir_to_string(dir) <> "_BRACE"
    EqualEqual -> "EQUAL_EQUAL"
    BangEqual -> "BANG_EQUAL"
    _ -> token |> string.inspect |> string.uppercase
  }
  <> " "
  <> token_to_pattern(token)
  <> " null"
}

pub type Return {
  Return(out: String, error: String)
}

pub fn scan(in: String) -> Return {
  let matcher =
    all_tokens
    |> list.map(match_token)
    |> any
    |> label_error(fn(in) {
      let unexpected = in |> string.first |> result.unwrap("")
      "[line 1] Error: Unexpected character: " <> unexpected
    })

  tokenized_to_return(tokenize(in, matcher))
}

fn match_token(token: Token) -> Lexer(Token) {
  fn(in) {
    let pattern = token_to_pattern(token)
    use #(in, _) <- result.map(tag(pattern)(in))
    #(in, token)
  }
}

type Tokenized {
  Token(Token)
  Unexpected(line: Int, char: String)
  Eof
}

fn tokenized_is_error(tokenized) -> Bool {
  case tokenized {
    Unexpected(..) -> True
    _ -> False
  }
}

fn tokenized_to_string(tokenized: Tokenized) -> String {
  case tokenized {
    Token(token) -> token_to_string(token)
    Unexpected(line, char) ->
      "[line "
      <> int.to_string(line)
      <> "] Error: Unexpected character: "
      <> char
    Eof -> "EOF  null"
  }
}

fn tokenized_to_return(tokenized: List(Tokenized)) -> Return {
  do_tokenized_to_return(tokenized, Return("", ""))
}

fn do_tokenized_to_return(tokenized: List(Tokenized), return: Return) -> Return {
  case tokenized {
    [] -> return
    [first, ..rest] -> {
      let stringified = tokenized_to_string(first)
      let return = case tokenized_is_error(first) {
        True -> Return(..return, error: stringified <> "\n" <> return.error)
        _ -> Return(..return, out: stringified <> "\n" <> return.out)
      }
      do_tokenized_to_return(rest, return)
    }
  }
}

fn tokenize(in: String, matcher: Lexer(Token)) -> List(Tokenized) {
  do_tokenize(in, matcher, [])
}

fn do_tokenize(
  in: String,
  matcher: Lexer(Token),
  tokenized: List(Tokenized),
) -> List(Tokenized) {
  case in {
    "" -> [Eof, ..tokenized]
    _ -> {
      case matcher(in) {
        Ok(#(in, token)) -> {
          do_tokenize(in, matcher, [Token(token), ..tokenized])
        }
        Error(_) -> {
          let assert Ok(first) = string.first(in)
          let in = string.drop_left(in, 1)
          do_tokenize(in, matcher, [Unexpected(1, first), ..tokenized])
        }
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

fn or(first: Lexer(a), second: Lexer(a)) -> Lexer(a) {
  fn(in) {
    use first_error <- result.try_recover(first(in))
    use second_error <- result.try_recover(second(in))
    Error(OrError(first_error, second_error))
  }
}

fn any(lexers: List(Lexer(a))) -> Lexer(a) {
  any_inner(_, lexers)
}

fn any_inner(in: String, lexers: List(Lexer(a))) -> LexResult(a) {
  case lexers {
    [] -> Error(AnyError)
    [first, ..rest] -> {
      use _ <- result.try_recover(first(in))
      any_inner(in, rest)
    }
  }
}

fn label_error(lexer: Lexer(a), to_label: fn(String) -> String) -> Lexer(a) {
  fn(in) { lexer(in) |> result.map_error(Labelled(_, to_label(in))) }
}
