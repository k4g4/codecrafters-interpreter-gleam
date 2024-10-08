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

type Tree {
  Node(Node, List(Tree))
  Leaf(common.Token)
}

fn tree_to_string(tree: Tree) -> String {
  case tree {
    Node(node, trees) -> {
      let stringified =
        trees
        |> list.map(tree_to_string)
        |> list.prepend(node_to_string(node))
        |> string.join(" ")
      "(" <> stringified <> ")"
    }
    Leaf(token) -> token_to_string(token)
  }
}

type Trees =
  List(Tree)

fn trees_to_string(trees: Trees) -> String {
  trees |> list.map(tree_to_string) |> string.join(" ")
}

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

pub fn parse(tokens: Tokens) -> common.Return {
  case do_parse(tokens) {
    Ok(trees) -> common.Return(out: trees_to_string(trees), error: "")
    Error(error) -> common.Return(out: "", error: parse_error_to_string(error))
  }
}

fn do_parse(tokens: Tokens) -> Result(Trees, ParseError) {
  use #(tokens, flat) <- result.try(flat(tokens))
  use <- bool.guard(tokens != [], Error(UnexpectedInput))
  let stages = [grouped, prefixed, infixed]
  list.try_fold(stages, flat, fn(trees, stage) {
    traverse_trees(stage)(trees) |> result.map(pair.second)
  })
}

fn flat(tokens: Tokens) -> Result(#(Tokens, Trees), ParseError) {
  collect(leaf)(tokens)
}

fn grouped(trees: Trees) -> Result(#(Trees, Tree), ParseError) {
  any([map(parens, Node(Group, _)), take_one])(trees)
}

fn parens(trees: Trees) -> Result(#(Trees, Trees), ParseError) {
  let paren = fn(dir) { one(Leaf(common.Basic(common.Paren(dir)))) }
  enclosed(paren(common.Left), grouped, paren(common.Right))(trees)
}

fn prefixed(trees: Trees) -> Result(#(Trees, Tree), ParseError) {
  let parser = fn(basic, prefix_type) {
    let pre = one(Leaf(common.Basic(basic)))
    map(prefix(pre, take(1)), Node(Prefix(prefix_type), _))
  }
  any([parser(common.Bang, Not), parser(common.Minus, Neg)])(trees)
}

fn infixed(trees: Trees) -> Result(#(Trees, Tree), ParseError) {
  let parser = fn(basic, infix_type) {
    let mid = one(Leaf(common.Basic(basic)))
    use #(left, right) <- map(infix(take_one, mid, take_one))
    Node(Infix(infix_type), [left, right])
  }
  any([
    parser(common.Star, Mul),
    parser(common.Slash, Div),
    parser(common.Plus, Add),
    parser(common.Minus, Sub),
  ])(trees)
}

fn traverse_trees(parser: Parser(Tree, Tree)) -> Parser(Tree, Trees) {
  fn(in) {
    let parser =
      parser
      |> maybe
      |> recover(take_one)
      |> collect
    let traverse = fn(trees) {
      use #(trees, tree) <- result.try(take_one(trees))
      case tree {
        Node(node, in) -> {
          use #(_, out) <- result.map(parser(in))
          #(trees, Node(node, out))
        }
        Leaf(token) -> Ok(#(trees, Leaf(token)))
      }
    }
    use #(in, out) <- result.try(collect(traverse)(in))
    use #(trees, converted) <- result.try(parser(out))
    use <- bool.guard(trees != [], Error(UnexpectedInput))
    Ok(#(in, converted))
  }
}

fn leaf(tokens: Tokens) -> Result(#(Tokens, Tree), ParseError) {
  case tokens {
    [token, ..tokens] -> Ok(#(tokens, Leaf(token)))
    _ -> Error(ExhaustedInput)
  }
}

fn maybe(parser: Parser(in, out)) -> Parser(in, Result(out, Nil)) {
  fn(in) {
    case parser(in) {
      Ok(#(in, out)) -> Ok(#(in, Ok(out)))
      _ -> Ok(#(in, Error(Nil)))
    }
  }
}

fn recover(
  try: Parser(in, Result(out, _)),
  recover: Parser(in, out),
) -> Parser(in, out) {
  fn(in) {
    use #(in, res) <- result.try(try(in))
    case res {
      Ok(out) -> Ok(#(in, out))
      _ -> recover(in)
    }
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

fn prefix(pre: Parser(in, _), parser: Parser(in, out)) -> Parser(in, out) {
  fn(in) {
    use #(in, _) <- result.try(pre(in))
    parser(in)
  }
}

fn infix(
  pre: Parser(in, left),
  mid: Parser(in, _),
  post: Parser(in, right),
) -> Parser(in, #(left, right)) {
  fn(in) {
    use #(in, left) <- result.try(pre(in))
    use #(in, _) <- result.try(mid(in))
    use #(in, right) <- result.map(post(in))
    #(in, #(left, right))
  }
}
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
