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

type Tree(phase) {
  Node(Node, List(Tree(phase)))
  Leaf(common.Token)
}

fn tree_to_string(tree: Tree(_)) -> String {
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

type Trees(phase) =
  List(Tree(phase))

fn trees_to_string(trees: Trees(_)) -> String {
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

type TreesParser(in, out) =
  Parser(Tree(in), Trees(out))

pub fn parse(tokens: Tokens) -> common.Return {
  case do_parse(tokens) {
    Ok(trees) -> common.Return(out: trees_to_string(trees), error: "")
    Error(error) -> common.Return(out: "", error: parse_error_to_string(error))
  }
}

import gleam/io

fn do_parse(tokens: Tokens) -> Result(Trees(Finished), ParseError) {
  use #(tokens, flat_trees) <- result.try(flat_trees(tokens))
  use <- bool.guard(tokens != [], Error(UnexpectedInput))
  use grouped_trees <- result.try(traverse_trees(flat_trees, grouped))
  io.debug(grouped_trees)
  // use prefixes_tree <- result.try(prefixes_tree(grouped_tree))
  // use infixes_tree <- result.try(infixes_tree(prefixes_tree))
  // finished_tree(infixes_tree)
  Ok(grouped_trees)
}

fn flat_trees(tokens: Tokens) -> Result(#(Tokens, Trees(Flat)), ParseError) {
  collect(leaf)(tokens)
}

fn grouped(
  trees: Trees(Flat),
) -> Result(#(Trees(Flat), Trees(Grouped)), ParseError) {
  collect(group_or_leaf)(trees)
}

fn group(
  trees: Trees(Flat),
) -> Result(#(Trees(Flat), Trees(Grouped)), ParseError) {
  let paren = fn(dir) { one(Leaf(common.Basic(common.Paren(dir)))) }
  enclosed(paren(common.Left), group_or_leaf, paren(common.Right))(trees)
}

fn group_or_leaf(
  trees: Trees(Flat),
) -> Result(#(Trees(Flat), Tree(Grouped)), ParseError) {
  any([
    map(group, Node(Group, _)),
    map(take_one, fn(tree) {
      case tree {
        Leaf(token) -> Leaf(token)
        _ -> panic as "trees should be Flat"
      }
    }),
  ])(trees)
}

fn traverse_trees(
  trees: Trees(in),
  parser: TreesParser(in, out),
) -> Result(Trees(out), ParseError) {
  do_traverse_trees(trees, parser, [])
}

fn do_traverse_trees(
  trees: Trees(in),
  parser: TreesParser(in, out),
  acc: Trees(out),
) -> Result(Trees(out), ParseError) {
  case trees {
    [tree, ..trees] -> {
      use tree <- result.try(traverse_tree(tree, parser))
      do_traverse_trees(trees, parser, [tree, ..acc])
    }
    _ -> Ok(list.reverse(acc))
  }
}

fn traverse_tree(
  tree: Tree(in),
  parser: TreesParser(in, out),
) -> Result(Tree(out), ParseError) {
  case tree {
    Node(node, trees) ->
      trees
      |> traverse_trees(parser)
      |> result.map(Node(node, _))
    Leaf(token) -> Ok(Leaf(token))
  }
}

fn leaf(tokens: Tokens) -> Result(#(Tokens, Tree(Flat)), ParseError) {
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
