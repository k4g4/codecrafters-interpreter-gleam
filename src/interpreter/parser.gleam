import interpreter/common.{type Return, Return}
import interpreter/lexer.{
  type Token, Keyword, KeywordFalse, KeywordNil, KeywordTrue,
}

pub fn parse(tokens: List(Token)) -> Return {
  case tokens {
    [Keyword(KeywordTrue, _), ..] -> Return(out: "true", error: "")
    [Keyword(KeywordFalse, _), ..] -> Return(out: "false", error: "")
    [Keyword(KeywordNil, _), ..] -> Return(out: "nil", error: "")
    _ -> Return(out: "", error: "unknown")
  }
}
