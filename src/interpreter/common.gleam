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

pub type Dir {
  Left
  Right
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
