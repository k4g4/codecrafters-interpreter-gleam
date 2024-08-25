import interpreter/lexer.{Return}

import gleam/io
import gleam/result

import argv
import simplifile

pub fn main() {
  let args = argv.load().arguments

  case args {
    ["tokenize", filename] -> {
      filename
      |> simplifile.read
      |> result.map(run)
      |> result.map_error(fn(error) {
        io.println_error("Error: " <> simplifile.describe_error(error))
        exit(1)
      })
      |> result.unwrap_both
    }
    ["debug", in] -> {
      run(in)
    }
    _ -> {
      io.println_error("Usage: ./your_program.sh tokenize <filename>")
      exit(1)
    }
  }
}

fn run(in: String) -> Nil {
  let Return(out, error) = lexer.scan(in)
  io.println_error(error)
  io.println(out)
  exit(case error {
    "" -> 0
    _ -> 65
  })
}

@external(erlang, "erlang", "halt")
pub fn exit(code: Int) -> Nil
