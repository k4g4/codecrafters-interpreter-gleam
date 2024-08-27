import gleam/bool
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

type Prefix {
  Neg
  Not
}

type Infix {
  Div
  Mul
  Sub
  Add
}

type Node {
  Flat
  Group
  Prefix(Prefix)
  Infix(Infix)
}

fn node_to_string(node: Node) -> String {
  case node {
    Flat -> ""
    Group -> "group"
    Prefix(Neg) -> "-"
    Prefix(Not) -> "!"
    Infix(Mul) -> "*"
    Infix(Div) -> "/"
    Infix(Add) -> "+"
    Infix(Sub) -> "-"
  }
}

type Flat

type Grouped

// type Prefixes

// type Infixes

type Finished =
  Grouped

type TokenTree(phase) {
  Node(Node, List(TokenTree(phase)))
  Leaf(common.Token)
}

fn token_tree_to_string(tree: TokenTree(_)) -> String {
  case tree {
    Node(node, trees) -> {
      let stringified =
        trees
        |> list.map(token_tree_to_string)
        |> list.prepend(node_to_string(node))
        |> string.join(" ")
      "(" <> stringified <> ")"
    }
    Leaf(token) -> token_to_string(token)
  }
}

type Trees(phase) =
  List(TokenTree(phase))

type ParseError {
  ExhaustedInput
  Expected(String)
  ExhaustedParsers
  UnexpectedInput
}

fn parse_error_to_string(error: ParseError) -> String {
  case error {
    ExhaustedInput -> "parser expected more input"
    Expected(item) -> "expected input: " <> item
    ExhaustedParsers -> "exhausted all parsers"
    UnexpectedInput -> "unexpected input while parsing"
  }
}

type Parser(in, out) =
  fn(List(in)) -> Result(#(List(in), out), ParseError)

type TreesParser(a) =
  Parser(TokenTree(a), Trees(a))

import gleam/io

pub fn parse(tokens: Tokens) -> common.Return {
  case do_parse(tokens) |> io.debug {
    Ok(tree) -> common.Return(out: token_tree_to_string(tree), error: "")
    Error(error) -> common.Return(out: "", error: parse_error_to_string(error))
  }
}

fn do_parse(tokens: Tokens) -> Result(TokenTree(Finished), ParseError) {
  use #(tokens, flat_tree) <- result.try(flat_tree(tokens))
  use <- bool.guard(tokens != [], Error(UnexpectedInput))
  use grouped_tree <- result.try(traverse_token_tree(flat_tree, grouped))
  // use prefixes_tree <- result.try(prefixes_tree(grouped_tree))
  // use infixes_tree <- result.try(infixes_tree(prefixes_tree))
  // finished_tree(infixes_tree)
  Ok(grouped_tree)
}

fn flat_tree(tokens: Tokens) -> Result(#(Tokens, TokenTree(Flat)), ParseError) {
  tokens |> collect(token_leaf) |> result.map(pair.map_second(_, Node(Flat, _)))
}

fn grouped(
  trees: Trees(Grouped),
) -> Result(#(Trees(Grouped), Trees(Grouped)), ParseError) {
  let paren = fn(dir) { one(Leaf(common.Basic(common.Paren(dir)))) }
  let group = enclosed(paren(common.Left), take_one, paren(common.Right))
  let group_or_leaf = any([map(group, Node(Group, _)), take_one])
  collect(group_or_leaf)(trees)
}

// fn negate(tokens: Tokens) -> Result(#(Tokens, TokenTree), ParseError) {
//   let parser = prefix(token(common.Basic(common.Minus)), token_tree)
//   use #(tokens, token_tree) <- result.map(parser(tokens))
//   #(tokens, Node(Neg, [token_tree]))
// }

// fn not(tokens: Tokens) -> Result(#(Tokens, TokenTree), ParseError) {
//   let parser = prefix(token(common.Basic(common.Bang)), token_tree)
//   use #(tokens, token_tree) <- result.map(parser(tokens))
//   #(tokens, Node(Not, [token_tree]))
// }

// fn matched_operators(
//   operators: List(#(TreeType, Parser(_))),
// ) -> Parser(TokenTree) {
//   fn(tokens) {
//     let operators_parser = one_of(operators)
//     let parser = three(token_tree, operators_parser, token_tree)
//     use #(tokens, #(left, #(operator, _), right)) <- result.map(parser(tokens))
//     #(tokens, Node(operator, [left, right]))
//   }
// }

// fn mul_div(tokens: Tokens) -> Result(#(Tokens, TokenTree), ParseError) {
//   matched_operators([
//     #(Mul, token(common.Basic(common.Star))),
//     #(Div, token(common.Basic(common.Slash))),
//   ])(tokens)
// }

// fn add_sub(tokens: Tokens) -> Result(#(Tokens, TokenTree), ParseError) {
//   matched_operators([
//     #(Add, token(common.Basic(common.Plus))),
//     #(Sub, token(common.Basic(common.Minus))),
//   ])(tokens)
// }

fn traverse_token_tree(
  tree: TokenTree(a),
  parser: TreesParser(b),
) -> Result(TokenTree(b), ParseError) {
  case tree {
    Node(node, trees) ->
      trees
      |> list.try_map(traverse_token_tree(_, parser))
      |> result.try(parser)
      |> result.map(pair.second)
      |> result.map(Node(node, _))
    Leaf(token) -> Ok(Leaf(token))
  }
}

fn token_leaf(tokens: Tokens) -> Result(#(Tokens, TokenTree(Flat)), ParseError) {
  case tokens {
    [token, ..tokens] -> Ok(#(tokens, Leaf(token)))
    _ -> Error(ExhaustedInput)
  }
}

fn take(i: Int) -> Parser(in, List(in)) {
  fn(in) {
    case list.length(in) >= i {
      True -> in |> list.split(i) |> pair.swap |> Ok
      _ -> Error(ExhaustedInput)
    }
  }
}

fn take_one(in: List(in)) -> Result(#(List(in), in), ParseError) {
  use #(in, wrapped_out) <- result.map(take(1)(in))
  let assert [out] = wrapped_out
  #(in, out)
}

fn one(item: a) -> Parser(a, a) {
  fn(in) {
    case in {
      [first, ..rest] if first == item -> Ok(#(rest, item))
      _ -> Error(ExhaustedInput)
    }
  }
}

fn any(parsers: List(Parser(in, out))) -> Parser(in, out) {
  fn(in) { any_inner(parsers, in) }
}

fn any_inner(
  parsers: List(Parser(in, out)),
  in: List(in),
) -> Result(#(List(in), out), ParseError) {
  case parsers {
    [] -> Error(ExhaustedParsers)
    [parser, ..parsers] -> {
      use _ <- result.try_recover(parser(in))
      any_inner(parsers, in)
    }
  }
}

fn collect(parser: Parser(in, out)) -> Parser(in, List(out)) {
  fn(in) { do_collect(parser, in, []) }
}

fn do_collect(
  parser: Parser(in, out),
  in: List(in),
  acc: List(out),
) -> Result(#(List(in), List(out)), ParseError) {
  case parser(in) {
    Ok(#(in, out)) -> do_collect(parser, in, [out, ..acc])
    _ -> Ok(#(in, list.reverse(acc)))
  }
}

fn enclosed(
  left: Parser(in, _),
  middle: Parser(in, out),
  right: Parser(in, _),
) -> Parser(in, List(out)) {
  fn(in) {
    use #(in, _) <- result.try(left(in))
    enclosed_inner(middle, right, in, [])
    |> result.map(pair.map_second(_, list.reverse))
  }
}

fn enclosed_inner(
  middle: Parser(in, out),
  right: Parser(in, _),
  in: List(in),
  acc: List(out),
) -> Result(#(List(in), List(out)), ParseError) {
  case right(in) {
    Ok(#(in, _)) -> Ok(#(in, acc))
    _ -> {
      use #(in, out) <- result.try(middle(in))
      enclosed_inner(middle, right, in, [out, ..acc])
    }
  }
}

fn map(
  parser: Parser(in, old_out),
  f: fn(old_out) -> new_out,
) -> Parser(in, new_out) {
  fn(in) {
    use #(in, out) <- result.map(parser(in))
    #(in, f(out))
  }
}
// fn prefix(pre: Parser(in, _), parser: Parser(in, out)) -> Parser(in, out) {
//   fn(in) {
//     use #(in, _) <- result.try(pre(in))
//     parser(in)
//   }
// }

// fn one_of(parsers: List(#(tag, Parser(in, out)))) -> Parser(in, #(tag, out)) {
//   fn(in) { one_of_inner(parsers, in) }
// }

// fn one_of_inner(
//   parsers: List(#(tag, Parser(in, out))),
//   in: List(in),
// ) -> Result(#(in, #(tag, out)), ParseError) {
//   case parsers {
//     [] -> Error(ExhaustedParsers)
//     [#(tag, parser), ..parsers] ->
//       case parser(in) {
//         Ok(#(in, out)) -> Ok(#(in, #(tag, out)))
//         _ -> one_of_inner(parsers, in)
//       }
//   }
// }

// fn three(
//   left: Parser(a),
//   middle: Parser(b),
//   right: Parser(c),
// ) -> Parser(#(a, b, c)) {
//   fn(tokens) {
//     let last_parsed_at_result =
//       last_parsed_at(tokens, middle, 0, Error(ExhaustedTokens))
//     use parsed_at <- result.try(last_parsed_at_result)
//     let left_tokens = list.take(tokens, parsed_at)
//     let tokens = list.drop(tokens, parsed_at)
//     use #(left_tokens, left) <- result.try(left(left_tokens))
//     use <- bool.guard(left_tokens != [], Error(UnexpectedTokens))
//     use #(tokens, middle) <- result.try(middle(tokens))
//     use #(tokens, right) <- result.try(right(tokens))
//     Ok(#(tokens, #(left, middle, right)))
//   }
// }

// fn last_parsed_at(
//   tokens: Tokens,
//   parser: Parser(_),
//   i: Int,
//   acc: Result(Int, ParseError),
// ) -> Result(Int, ParseError) {
//   case parser(tokens) {
//     Ok(_) -> last_parsed_at(list.drop(tokens, 1), parser, i + 1, Ok(i))
//     _ ->
//       case tokens {
//         [] -> acc
//         [_, ..tokens] -> last_parsed_at(tokens, parser, i + 1, acc)
//       }
//   }
// }
