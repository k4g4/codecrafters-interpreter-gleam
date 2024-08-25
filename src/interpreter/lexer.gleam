import gleam/bool
import gleam/list
import gleam/result
import gleam/string

type LexError {
  TagError(String)
  OrError(LexError, LexError)
  AnyError
}

fn error_to_string(error: LexError) -> String {
  case error {
    TagError(tag) -> "tag(" <> tag <> ")"
    OrError(first, second) ->
      "(" <> error_to_string(first) <> ", " <> error_to_string(second) <> ")"
    AnyError -> "any"
  }
}

type LexResult(a) =
  Result(#(String, a), LexError)

type Lexer(a) =
  fn(String) -> LexResult(a)

type Token {
  LeftParen
  RightParen
  LeftBrace
  RightBrace
}

fn token_to_pattern(token: Token) -> String {
  case token {
    LeftParen -> "("
    RightParen -> ")"
    LeftBrace -> "{"
    RightBrace -> "}"
  }
}

const all_tokens = [LeftParen, RightParen, LeftBrace, RightBrace]

fn token_to_string(token: Token) -> String {
  case token {
    LeftParen -> "LEFT_PAREN ( null"
    RightParen -> "RIGHT_PAREN ) null"
    LeftBrace -> "LEFT_BRACE { null"
    RightBrace -> "RIGHT_BRACE } null"
  }
  <> "\n"
}

fn tokens_to_string(tokens: List(Token), acc: String) -> String {
  case tokens {
    [] -> acc
    [first, ..rest] -> {
      tokens_to_string(rest, token_to_string(first) <> acc)
    }
  }
}

pub fn scan(in: String) -> Result(String, String) {
  let matcher = all_tokens |> list.map(match_token) |> any
  tokenize(in, matcher, [])
  |> result.map_error(error_to_string)
  |> result.map(tokens_to_string(_, ""))
  |> result.map(string.append(_, "EOF  null"))
}

fn match_token(token: Token) -> Lexer(Token) {
  fn(in) {
    let pattern = token_to_pattern(token)
    use #(in, _) <- result.map(tag(pattern)(in))
    #(in, token)
  }
}

fn tokenize(
  in: String,
  matcher: Lexer(Token),
  tokens: List(Token),
) -> Result(List(Token), LexError) {
  use <- bool.guard(string.is_empty(in), Ok(tokens))
  use #(in, token) <- result.try(matcher(in))
  tokenize(in, matcher, [token, ..tokens])
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
