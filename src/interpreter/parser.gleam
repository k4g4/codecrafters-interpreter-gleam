import interpreter/common

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
}

fn tree_type_to_string(tree_type: TreeType) -> String {
  case tree_type {
    Group -> "group"
    Negate -> "-"
    Not -> "!"
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
}

fn parse_error_to_string(error: ParseError) -> String {
  case error {
    ExhaustedTokens -> "expected more tokens"
    ExpectedToken(token) -> "expected token: " <> common.token_to_string(token)
    ExhaustedParsers -> "exhausted all parsers"
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
  any([group, negate, not, token_leaf])(tokens)
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
  left: Parser(a),
  middle: Parser(b),
  right: Parser(c),
) -> Parser(List(b)) {
  fn(tokens) {
    use #(tokens, _) <- result.try(left(tokens))
    enclosed_inner(middle, right, tokens, [])
    |> result.map(pair.map_second(_, list.reverse))
  }
}

fn enclosed_inner(
  middle: Parser(a),
  right: Parser(b),
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

fn prefix(pre: Parser(a), parser: Parser(b)) -> Parser(b) {
  fn(tokens) {
    use #(tokens, _) <- result.try(pre(tokens))
    parser(tokens)
  }
}
