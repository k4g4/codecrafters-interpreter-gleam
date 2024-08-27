import interpreter/common

import gleam/bool
import gleam/float
import gleam/list
import gleam/pair
import gleam/result
import gleam/string

fn token_to_string(token: common.Token) -> String {
  case token {
    common.Ident(ident) -> ident
    common.Literal(common.LiteralString, lexeme) | common.Keyword(_, lexeme) ->
      lexeme
    common.Literal(common.LiteralNumber(number), _) -> float.to_string(number)
    common.Comment -> ""
    common.Basic(basic_token) -> common.basic_token_to_pattern(basic_token)
  }
}

type Tokens =
  List(common.Token)

type TokenTree {
  Node(TreeType, List(TokenTree))
  Leaf(common.Token)
}

type TreeType {
  Group
  Negate
  Not
  Div
  Mul
}

fn tree_type_to_string(tree_type: TreeType) -> String {
  case tree_type {
    Group -> "group"
    Negate -> "-"
    Not -> "!"
    Div -> "/"
    Mul -> "*"
  }
}

fn token_tree_to_string(token_tree: TokenTree) -> String {
  case token_tree {
    Node(tree_type, token_trees) -> {
      let stringified =
        token_trees
        |> list.map(token_tree_to_string)
        |> list.prepend(tree_type_to_string(tree_type))
        |> string.join(" ")
      "(" <> stringified <> ")"
    }
    Leaf(token) -> token_to_string(token)
  }
}

type ParseError {
  ExhaustedTokens
  ExpectedToken(common.Token)
  ExhaustedParsers
  UnexpectedTokens
}

fn parse_error_to_string(error: ParseError) -> String {
  case error {
    ExhaustedTokens -> "expected more tokens"
    ExpectedToken(token) -> "expected token: " <> common.token_to_string(token)
    ExhaustedParsers -> "exhausted all parsers"
    UnexpectedTokens -> "unexpected tokens while parsing"
  }
}

type Parser(a) =
  fn(Tokens) -> Result(#(Tokens, a), ParseError)

pub fn parse(tokens: Tokens) -> common.Return {
  case token_tree(tokens) {
    Ok(#([], token_tree)) ->
      common.Return(out: token_tree_to_string(token_tree), error: "")
    Ok(_) -> common.Return(out: "", error: "tokens not exhausted")
    Error(error) -> common.Return(out: "", error: parse_error_to_string(error))
  }
}

fn token_tree(tokens: Tokens) -> Result(#(Tokens, TokenTree), ParseError) {
  any([group, negate, not, div, mul, token_leaf])(tokens)
}

fn group(tokens: Tokens) -> Result(#(Tokens, TokenTree), ParseError) {
  let left = token(common.Basic(common.Paren(common.Left)))
  let right = token(common.Basic(common.Paren(common.Right)))
  tokens
  |> enclosed(left, token_tree, right)
  |> result.map(pair.map_second(_, Node(Group, _)))
}

fn negate(tokens: Tokens) -> Result(#(Tokens, TokenTree), ParseError) {
  let prefix_result =
    prefix(token(common.Basic(common.Minus)), token_tree)(tokens)
  use #(tokens, token_tree) <- result.map(prefix_result)
  #(tokens, Node(Negate, [token_tree]))
}

fn not(tokens: Tokens) -> Result(#(Tokens, TokenTree), ParseError) {
  let prefix_result =
    prefix(token(common.Basic(common.Bang)), token_tree)(tokens)
  use #(tokens, token_tree) <- result.map(prefix_result)
  #(tokens, Node(Not, [token_tree]))
}

fn div(tokens: Tokens) -> Result(#(Tokens, TokenTree), ParseError) {
  let infix_result =
    infix(token_tree, token(common.Basic(common.Slash)), token_tree)(tokens)
  use #(tokens, #(left, right)) <- result.map(infix_result)
  #(tokens, Node(Div, [left, right]))
}

fn mul(tokens: Tokens) -> Result(#(Tokens, TokenTree), ParseError) {
  let infix_result =
    infix(token_tree, token(common.Basic(common.Star)), token_tree)(tokens)
  use #(tokens, #(left, right)) <- result.map(infix_result)
  #(tokens, Node(Mul, [left, right]))
}

fn token_leaf(tokens: Tokens) -> Result(#(Tokens, TokenTree), ParseError) {
  case tokens {
    [] -> Error(ExhaustedTokens)
    [token, ..tokens] -> Ok(#(tokens, Leaf(token)))
  }
}

fn token(token: common.Token) -> Parser(Nil) {
  fn(tokens) {
    case tokens {
      [first, ..rest] if first == token -> Ok(#(rest, Nil))
      _ -> Error(ExpectedToken(token))
    }
  }
}

fn any(parsers: List(Parser(a))) -> Parser(a) {
  fn(tokens) { any_inner(parsers, tokens) }
}

fn any_inner(
  parsers: List(Parser(a)),
  tokens: Tokens,
) -> Result(#(Tokens, a), ParseError) {
  case parsers {
    [] -> Error(ExhaustedParsers)
    [parser, ..parsers] -> {
      use _ <- result.try_recover(parser(tokens))
      any_inner(parsers, tokens)
    }
  }
}

fn enclosed(
  left: Parser(_),
  middle: Parser(a),
  right: Parser(_),
) -> Parser(List(a)) {
  fn(tokens) {
    use #(tokens, _) <- result.try(left(tokens))
    enclosed_inner(middle, right, tokens, [])
    |> result.map(pair.map_second(_, list.reverse))
  }
}

fn enclosed_inner(
  middle: Parser(a),
  right: Parser(_),
  tokens: Tokens,
  acc: List(a),
) -> Result(#(Tokens, List(a)), ParseError) {
  case right(tokens) {
    Ok(#(tokens, _)) -> Ok(#(tokens, acc))
    _ -> {
      use #(tokens, item) <- result.try(middle(tokens))
      enclosed_inner(middle, right, tokens, [item, ..acc])
    }
  }
}

fn prefix(pre: Parser(_), parser: Parser(a)) -> Parser(a) {
  fn(tokens) {
    use #(tokens, _) <- result.try(pre(tokens))
    parser(tokens)
  }
}

fn infix(left: Parser(a), in: Parser(_), right: Parser(b)) -> Parser(#(a, b)) {
  fn(tokens) {
    let last_parsed_at_result =
      last_parsed_at(tokens, in, 0, Error(ExhaustedTokens))
    use parsed_at <- result.try(last_parsed_at_result)
    let left_tokens = list.take(tokens, parsed_at)
    let tokens = list.drop(tokens, parsed_at)
    use #(left_tokens, left) <- result.try(left(left_tokens))
    use <- bool.guard(left_tokens != [], Error(UnexpectedTokens))
    use #(tokens, _) <- result.try(in(tokens))
    use #(tokens, right) <- result.try(right(tokens))
    Ok(#(tokens, #(left, right)))
  }
}

fn last_parsed_at(
  tokens: Tokens,
  parser: Parser(_),
  i: Int,
  acc: Result(Int, ParseError),
) -> Result(Int, ParseError) {
  case parser(tokens) {
    Ok(_) -> last_parsed_at(list.drop(tokens, 1), parser, i + 1, Ok(i))
    _ ->
      case tokens {
        [] -> acc
        [_, ..tokens] -> last_parsed_at(tokens, parser, i + 1, acc)
      }
  }
}
