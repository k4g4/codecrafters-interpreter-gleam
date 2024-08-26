import interpreter/common

import gleam/float

pub fn parse(tokens: List(common.Token)) -> common.Return {
  case tokens {
    [common.Keyword(common.KeywordTrue, _), ..] ->
      common.Return(out: "true", error: "")
    [common.Keyword(common.KeywordFalse, _), ..] ->
      common.Return(out: "false", error: "")
    [common.Keyword(common.KeywordNil, _), ..] ->
      common.Return(out: "nil", error: "")
    [common.Literal(common.LiteralNumber(number), _), ..] ->
      common.Return(out: float.to_string(number), error: "")
    [common.Literal(common.LiteralString, lexeme), ..] ->
      common.Return(out: lexeme, error: "")
    _ -> common.Return(out: "", error: "unknown")
  }
}
