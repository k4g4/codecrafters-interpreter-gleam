import gleam/float
import gleam/string

pub type Return {
  Return(out: String, error: String)
}

pub type Token {
  Ident(String)
  Literal(literal: Literal, lexeme: String)
  Comment
  Keyword(KeywordToken, lexeme: String)
  Basic(BasicToken)
}

pub fn token_to_string(token: Token) -> String {
  case token {
    Ident(ident) -> "IDENTIFIER " <> ident <> " null"

    Literal(LiteralString, lexeme) -> "STRING \"" <> lexeme <> "\" " <> lexeme

    Literal(LiteralNumber(number), lexeme) ->
      "NUMBER " <> lexeme <> " " <> float.to_string(number)

    Comment -> ""

    Keyword(_, lexeme) -> string.uppercase(lexeme) <> " " <> lexeme <> " null"

    Basic(basic_token) -> {
      let name = case basic_token {
        Paren(dir) -> dir_to_string(dir) <> "_PAREN"
        Brace(dir) -> dir_to_string(dir) <> "_BRACE"
        EqualEqual -> "EQUAL_EQUAL"
        BangEqual -> "BANG_EQUAL"
        LessEqual -> "LESS_EQUAL"
        GreaterEqual -> "GREATER_EQUAL"
        basic_token -> basic_token |> string.inspect |> string.uppercase
      }
      name <> " " <> basic_token_to_pattern(basic_token) <> " null"
    }
  }
}

pub type Literal {
  LiteralString
  LiteralNumber(Float)
}

pub type KeywordToken {
  KeywordAnd
  KeywordClass
  KeywordElse
  KeywordFalse
  KeywordFor
  KeywordFun
  KeywordIf
  KeywordNil
  KeywordOr
  KeywordPrint
  KeywordReturn
  KeywordSuper
  KeywordThis
  KeywordTrue
  KeywordVar
  KeywordWhile
}

pub fn keyword_to_pattern(keyword: KeywordToken) -> String {
  keyword
  |> string.inspect
  |> string.drop_left(string.length("Keyword"))
  |> string.lowercase
}

pub type Dir {
  Left
  Right
}

fn dir_to_string(dir: Dir) -> String {
  case dir {
    Left -> "LEFT"
    _ -> "RIGHT"
  }
}

pub type BasicToken {
  Paren(Dir)
  Brace(Dir)
  EqualEqual
  BangEqual
  LessEqual
  GreaterEqual
  Space
  Tab
  Newline
  Equal
  Bang
  Less
  Greater
  Star
  Dot
  Comma
  Plus
  Minus
  Slash
  Semicolon
}

pub fn basic_token_to_pattern(basic_token: BasicToken) -> String {
  case basic_token {
    Paren(Left) -> "("
    Paren(Right) -> ")"
    Brace(Left) -> "{"
    Brace(Right) -> "}"
    EqualEqual -> "=="
    BangEqual -> "!="
    LessEqual -> "<="
    GreaterEqual -> ">="
    Space -> " "
    Tab -> "\t"
    Newline -> "\n"
    Equal -> "="
    Bang -> "!"
    Less -> "<"
    Greater -> ">"
    Star -> "*"
    Dot -> "."
    Comma -> ","
    Plus -> "+"
    Minus -> "-"
    Slash -> "/"
    Semicolon -> ";"
  }
}
