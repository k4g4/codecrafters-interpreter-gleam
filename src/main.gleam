import interpreter/common.{Return}
import interpreter/lexer
import interpreter/parser

import gleam/io
import gleam/result

import argv
import simplifile

pub fn main() {
  let args = argv.load().arguments
  let run = fn(filename, f) {
    filename
    |> simplifile.read
    |> result.map(f)
    |> result.map_error(fn(error) {
      io.println_error("Error: " <> simplifile.describe_error(error))
      exit(1)
    })
    |> result.unwrap_both
  }

  case args {
    ["tokenize", filename] -> {
      run(filename, tokenize)
    }
    ["tokenize", "debug", in] -> {
      tokenize(in)
    }
    ["parse", filename] -> {
      run(filename, parse)
    }
    ["parse", "debug", in] -> {
      parse(in)
    }
    _ -> {
      io.println_error("Usage: ./your_program.sh tokenize <filename>")
      exit(1)
    }
  }
}

fn tokenize(in: String) -> Nil {
  let Return(out, error) = lexer.scan(in)
  case error {
    "" -> Nil
    _ -> io.print_error(error)
  }
  case out {
    "" -> Nil
    _ -> io.print(out)
  }
  exit(case error {
    "" -> 0
    _ -> 65
  })
}

fn parse(in: String) -> Nil {
  case lexer.lex(in) {
    Error(error) -> {
      io.print_error(error)
      exit(1)
    }
    Ok(tokens) -> {
      let Return(out, error) = parser.parse(tokens)
      case error {
        "" -> Nil
        _ -> io.print_error(error)
      }
      case out {
        "" -> Nil
        _ -> io.print(out)
      }
      exit(case error {
        "" -> 0
        _ -> 1
      })
    }
  }
}

@external(erlang, "erlang", "halt")
pub fn exit(code: Int) -> Nil
